# Copyright (c) 2019 Sonian Inc.
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

require "sensu/daemon"
require "sensu/client/socket"
require "sensu/client/http_socket"

module Sensu
  module Client
    class Process
      include Daemon

      attr_accessor :safe_mode

      # Create an instance of the Sensu client process, start the
      # client within the EventMachine event loop, and set up client
      # process signal traps (for stopping).
      #
      # @param options [Hash]
      def self.run(options={})
        client = self.new(options)
        EM::run do
          client.start
          client.setup_signal_traps
        end
      end

      # Override Daemon initialize() to support Sensu client check
      # execution safe mode, checks in progress, and open sockets.
      #
      # @param options [Hash]
      def initialize(options={})
        super
        @safe_mode = @settings[:client][:safe_mode] || false
        @checks_in_progress = []
        @sockets = []
      end

      # Create a Sensu client keepalive payload, to be sent over the
      # transport for processing. A client keepalive is composed of
      # its settings definition, the Sensu version, and a timestamp.
      # Sensitive information is redacted from the keepalive payload.
      #
      # @return [Hash] keepalive payload
      def keepalive_payload
        payload = @settings[:client].merge({
          :version => VERSION,
          :timestamp => Time.now.to_i
        })
        redact_sensitive(payload, @settings[:client][:redact])
      end

      # Publish a Sensu client keepalive to the transport for
      # processing. JSON serialization is used for transport messages.
      def publish_keepalive
        payload = keepalive_payload
        @logger.debug("publishing keepalive", :payload => payload)
        @transport.publish(:direct, "keepalives", Sensu::JSON.dump(payload)) do |info|
          if info[:error]
            @logger.error("failed to publish keepalive", {
              :payload => payload,
              :error => info[:error].to_s
            })
          end
        end
      end

      # Schedule Sensu client keepalives. Immediately publish a
      # keepalive to register the client, then publish a keepalive
      # every 20 seconds. Sensu client keepalives are used to
      # determine client (& machine) health.
      def setup_keepalives
        @logger.debug("scheduling keepalives")
        publish_keepalive
        @timers[:run] << EM::PeriodicTimer.new(20) do
          publish_keepalive
        end
      end

      # Publish a check result to the transport for processing. A
      # check result is composed of a client (name) and a check
      # definition, containing check `:output` and `:status`. JSON
      # serialization is used when publishing the check result payload
      # to the transport pipe. The check result is signed with the
      # client signature if configured, for source validation.
      # Transport errors are logged.
      #
      # @param check [Hash]
      def publish_check_result(check)
        check.delete(:source) if check[:source] == ""
        payload = {
          :client => @settings[:client][:name],
          :check => check
        }
        payload[:signature] = @settings[:client][:signature] if @settings[:client][:signature]
        @logger.info("publishing check result", :payload => payload)
        @transport.publish(:direct, "results", Sensu::JSON.dump(payload)) do |info|
          if info[:error]
            @logger.error("failed to publish check result", {
              :payload => payload,
              :error => info[:error].to_s
            })
          end
        end
      end

      # Create an in progress key for a check, used to determine if an
      # execution is still in progress. The key is composed of check
      # `source` (if set) and `name`, joined by a colon.
      #
      # @param check [Hash]
      # @return [String]
      def check_in_progress_key(check)
        [check[:source], check[:name]].compact.join(":")
      end

      # Execute a check hook, capturing its output (STDOUT/ERR),
      # exit status code, executed timestamp, and duration. This
      # method determines which hook command to run by inspecting the
      # check execution status. Check hook command tokens are
      # substituted with the associated client attribute values, via
      # `substitute_tokens()`. If there are unmatched check attribute
      # value tokens, the check hook will not be executed, instead the
      # hook command output will be set to report the unmatched
      # tokens. Hook commands may expect/read and utilize JSON
      # serialized Sensu client and check data via STDIN, if the hook
      # definition includes `"stdin": true` (default is `false`). A
      # hook may have a configured execution timeout, e.g. `"timeout":
      # 30`, if one is not specified, the timeout defaults to 60
      # seconds.
      #
      # @param check [Hash]
      # @yield [check] callback/block called after executing the check
      #   hook (if any).
      def execute_check_hook(check)
        @logger.debug("attempting to execute check hook", :check => check)
        severity = SEVERITIES[check[:status]] || "unknown"
        hook = check[:hooks][check[:status].to_s.to_sym] || check[:hooks][severity.to_sym]
        if hook.nil? && check[:status] != 0
          hook = check[:hooks]["non-zero".to_sym]
        end
        if hook
          command, unmatched_tokens = substitute_tokens(hook[:command].dup, @settings[:client])
          started = Time.now.to_f
          hook[:executed] = started.to_i
          if unmatched_tokens.empty?
            options = {:timeout => hook.fetch(:timeout, 60)}
            if hook[:stdin]
              options[:data] = Sensu::JSON.dump({
                :client => @settings[:client],
                :check => check
              })
            end
            Spawn.process(command, options) do |output, status|
              hook[:duration] = ("%.3f" % (Time.now.to_f - started)).to_f
              hook[:output] = output
              hook[:status] = status
              yield(check)
            end
          else
            hook[:output] = "Unmatched client token(s): " + unmatched_tokens.join(", ")
            hook[:status] = 3
            yield(check)
          end
        else
          yield(check)
        end
      end

      # Execute a check command, capturing its output (STDOUT/ERR),
      # exit status code, execution duration, timestamp, and publish
      # the result. This method guards against multiple executions for
      # the same check. Check attribute value tokens are substituted
      # with the associated client attribute values, via
      # `object_substitute_tokens()`. The original check command and
      # hooks are always published, to guard against publishing
      # sensitive/redacted client attribute values. If there are
      # unmatched check attribute value tokens, the check will not be
      # executed, instead a check result will be published reporting
      # the unmatched tokens.
      #
      # @param check [Hash]
      def execute_check_command(check)
        @logger.debug("attempting to execute check command", :check => check)
        in_progress_key = check_in_progress_key(check)
        unless @checks_in_progress.include?(in_progress_key)
          @checks_in_progress << in_progress_key
          substituted, unmatched_tokens = object_substitute_tokens(check.dup, @settings[:client])
          check = substituted.merge(:command => check[:command], :hooks => check[:hooks])
          check.delete(:hooks) if check[:hooks].nil?
          started = Time.now.to_f
          check[:executed] = started.to_i
          if unmatched_tokens.empty?
            options = {:timeout => check[:timeout]}
            if check[:stdin]
              options[:data] = Sensu::JSON.dump({
                :client => @settings[:client],
                :check => check
              })
            end
            Spawn.process(substituted[:command], options) do |output, status|
              check[:duration] = ("%.3f" % (Time.now.to_f - started)).to_f
              check[:output] = output
              check[:status] = status
              if check[:hooks] && !check[:hooks].empty?
                execute_check_hook(check) do |check|
                  publish_check_result(check)
                  @checks_in_progress.delete(in_progress_key)
                end
              else
                publish_check_result(check)
                @checks_in_progress.delete(in_progress_key)
              end
            end
          else
            check[:output] = "Unmatched client token(s): " + unmatched_tokens.join(", ")
            check[:status] = 3
            check[:handle] = false
            publish_check_result(check)
            @checks_in_progress.delete(in_progress_key)
          end
        else
          @logger.warn("previous check command execution in progress", :check => check)
        end
      end

      # Run a check extension and publish the result. The Sensu client
      # loads check extensions, checks that run within the Sensu Ruby
      # VM and the EventMachine event loop, using the Sensu Extension
      # API. If a check definition includes `:extension`, use it's
      # value for the extension name, otherwise use the check name.
      # The check definition is passed to the extension `safe_run()`
      # method as a parameter, the extension may utilize it. This
      # method guards against multiple executions for the same check
      # extension.
      #
      # https://github.com/sensu/sensu-extension
      #
      # @param check [Hash]
      def run_check_extension(check)
        @logger.debug("attempting to run check extension", :check => check)
        in_progress_key = check_in_progress_key(check)
        unless @checks_in_progress.include?(in_progress_key)
          @checks_in_progress << in_progress_key
          started = Time.now.to_f
          check[:executed] = started.to_i
          extension_name = check[:extension] || check[:name]
          extension = @extensions[:checks][extension_name]
          extension.safe_run(check) do |output, status|
            check[:duration] = ("%.3f" % (Time.now.to_f - started)).to_f
            check[:output] = output
            check[:status] = status
            if check[:hooks] && !check[:hooks].empty?
              execute_check_hook(check) do |check|
                publish_check_result(check)
                @checks_in_progress.delete(in_progress_key)
              end
            else
              publish_check_result(check)
              @checks_in_progress.delete(in_progress_key)
            end
          end
        else
          @logger.warn("previous check extension execution in progress", :check => check)
        end
      end

      # Process a check request. If a check request has a check
      # command, it will be executed. A standard check request will be
      # merged with a local check definition, if present. Client safe
      # mode is enforced in this method, requiring a local check
      # definition in order to execute the check command. If a local
      # check definition does not exist when operating with client
      # safe mode, a check result will be published to report the
      # missing check definition. A check request without a
      # command indicates a check extension run. The check request may
      # contain `:extension`, the name of the extension to run. If
      # `:extension` is not present, the check name is used for the
      # extension name. If a check extension does not exist for a
      # name, a check result will be published to report the unknown
      # check extension.
      #
      # @param check [Hash]
      def process_check_request(check)
        @logger.debug("processing check", :check => check)
        if @settings.check_exists?(check[:name]) && !check.has_key?(:proxy_requests)
          check.merge!(@settings[:checks][check[:name]])
        end
        if check.has_key?(:command)
          if @safe_mode && !@settings.check_exists?(check[:name])
            check[:output] = "Check is not locally defined (safe mode)"
            check[:status] = 3
            check[:handle] = false
            check[:executed] = Time.now.to_i
            publish_check_result(check)
          else
            execute_check_command(check)
          end
        else
          extension_name = check[:extension] || check[:name]
          if @extensions.check_exists?(extension_name)
            run_check_extension(check)
          else
            @logger.warn("unknown check extension", :check => check)
          end
        end
      end

      # Determine the Sensu transport subscribe options for a
      # subscription. If a subscription begins with a transport pipe
      # type, either "direct:" or "roundrobin:", the subscription uses
      # a direct transport pipe, and the subscription name is used for
      # both the pipe and the funnel names. If a subscription does not
      # specify a transport pipe type, a fanout transport pipe is
      # used, the subscription name is used for the pipe, and a unique
      # funnel is created for the Sensu client. The unique funnel name
      # for the Sensu client is created using a combination of the
      # client name, the Sensu version, and the process start time
      # (epoch).
      #
      # @param subscription [String]
      # @return [Array] containing the transport subscribe options:
      #   the transport pipe type, pipe, and funnel.
      def transport_subscribe_options(subscription)
        _, raw_type = subscription.split(":", 2).reverse
        case raw_type
        when "direct", "roundrobin"
          [:direct, subscription, subscription]
        else
          funnel = [@settings[:client][:name], VERSION, start_time].join("-")
          [:fanout, subscription, funnel]
        end
      end

      # Set up Sensu client subscriptions. Subscriptions determine the
      # kinds of check requests the client will receive. The Sensu
      # client will receive JSON serialized check requests from its
      # subscriptions, that get parsed and processed.
      def setup_subscriptions
        @logger.debug("subscribing to client subscriptions")
        @settings[:client][:subscriptions].each do |subscription|
          @logger.debug("subscribing to a subscription", :subscription => subscription)
          options = transport_subscribe_options(subscription)
          @transport.subscribe(*options) do |message_info, message|
            begin
              check = Sensu::JSON.load(message)
              @logger.info("received check request", :check => check)
              process_check_request(check)
            rescue Sensu::JSON::ParseError => error
              @logger.error("failed to parse the check request payload", {
                :message => message,
                :error => error.to_s
              })
            end
          end
        end
      end

      # Create a check execution proc, used to execute standalone
      # checks. Checks are not executed if subdued. The check
      # `:issued` timestamp is set here, to mimic check requests
      # issued by a Sensu server. Check definitions are duplicated
      # before processing them, in case they are mutated.
      #
      # @param check [Hash] definition.
      def create_check_execution_proc(check)
        Proc.new do
          unless check_subdued?(check)
            check[:issued] = Time.now.to_i
            process_check_request(check.dup)
          else
            @logger.info("check execution was subdued", :check => check)
          end
        end
      end

      # Schedule a check execution, using the check cron. This method
      # determines the time until the next cron time (in seconds) and
      # creats an EventMachine timer for the execution. This method
      # will be called after every check cron execution for subsequent
      # executions. The timer is stored in the timers hash under
      # `:run`, so it can be cancelled etc. The check cron execution
      # timer object is removed from the timer hash after the
      # execution, to stop the timer hash from growing infinitely.
      #
      # @param check [Hash] definition.
      def schedule_check_cron_execution(check)
        cron_time = determine_check_cron_time(check)
        @timers[:run] << EM::Timer.new(cron_time) do |timer|
          create_check_execution_proc(check).call
          @timers[:run].delete(timer)
          schedule_check_cron_execution(check)
        end
      end

      # Calculate a check execution splay, taking into account the
      # current time and the execution interval to ensure it's
      # consistent between process restarts.
      #
      # @param check [Hash] definition.
      def calculate_check_execution_splay(check)
        key = [@settings[:client][:name], check[:name]].join(":")
        splay_hash = Digest::MD5.digest(key).unpack("Q<").first
        current_time = (Time.now.to_f * 1000).to_i
        (splay_hash - current_time) % (check[:interval] * 1000) / 1000.0
      end

      # Schedule check executions, using the check interval. This
      # method using an intial calculated execution splay EventMachine
      # timer and an EventMachine periodic timer for subsequent check
      # executions. The timers are stored in the timers hash under
      # `:run`, so they can be cancelled etc.
      #
      # @param check [Hash] definition.
      def schedule_check_interval_executions(check)
        execution_splay = testing? ? 0 : calculate_check_execution_splay(check)
        interval = testing? ? 0.5 : check[:interval]
        @timers[:run] << EM::Timer.new(execution_splay) do
          execute_check = create_check_execution_proc(check)
          execute_check.call
          @timers[:run] << EM::PeriodicTimer.new(interval, &execute_check)
        end
      end

      # Schedule check executions. This method iterates through defined
      # checks and uses the appropriate method of check execution
      # scheduling, either with the cron syntax or a numeric interval.
      #
      # @param checks [Array] of definitions.
      def schedule_checks(checks)
        checks.each do |check|
          if check[:cron]
            schedule_check_cron_execution(check)
          else
            schedule_check_interval_executions(check)
          end
        end
      end

      # Setup standalone check executions, scheduling standard check
      # definition and check extension executions. Check definitions
      # and extensions with `:standalone` set to `true`, do not have
      # `:publish` set to `false`, and have a integer `:interval` or a
      # string `cron` will be scheduled by the Sensu client for
      # execution.
      def setup_standalone
        @logger.debug("scheduling standalone checks")
        standard_checks = @settings.checks.select do |check|
          check[:standalone] && check[:publish] != false &&
            (check[:interval].is_a?(Integer) || check[:cron].is_a?(String))
        end
        extension_checks = @extensions.checks.select do |check|
          check[:standalone] && check[:publish] != false &&
            (check[:interval].is_a?(Integer) || check[:cron].is_a?(String))
        end
        schedule_checks(standard_checks + extension_checks)
      end

      # Setup the Sensu client JSON socket, for external check result
      # input. By default, the client socket is bound to localhost on
      # TCP & UDP port 3030. The socket can be configured via the
      # client definition, `:socket` with `:bind` and `:port`. Users can opt-out
      # of using the TCP and UDP socket by setting `:enabled` to `false`.
      # The current instance of the Sensu logger, settings, and transport
      # are passed to the socket handler, `Sensu::Client::Socket`. The
      # TCP socket server signature (Fixnum) and UDP connection object
      # are stored in `@sockets`, so that they can be managed
      # elsewhere, eg. `close_sockets()`.
      def setup_json_socket
        options = @settings[:client][:socket] || Hash.new
        options[:bind] ||= "127.0.0.1"
        options[:port] ||= 3030
        unless options[:enabled] == false
          @logger.debug("binding client tcp and udp sockets", :options => options)
          @sockets << EM::start_server(options[:bind], options[:port], Socket) do |socket|
            socket.logger = @logger
            socket.settings = @settings
            socket.transport = @transport
          end
          @sockets << EM::open_datagram_socket(options[:bind], options[:port], Socket) do |socket|
            socket.logger = @logger
            socket.settings = @settings
            socket.transport = @transport
            socket.protocol = :udp
          end
        else
          @logger.info("client tcp/udp socket disabled per configuration")
        end
      end

      # Setup the Sensu client HTTP socket, for external check result
      # input and informational queries. By default, the client HTTP
      # socket is bound to localhost on TCP port 3031. The socket can
      # be configured via the client definition, `:http_socket` with
      # `:bind` and `:port`. Users can opt-out of using the HTTP
      # socket by setting `:enabled` to `false. The current instance
      # of the Sensu logger, settings, and transport are passed to the
      # HTTP socket handler, `Sensu::Client::HTTPSocket`. The HTTP
      # socket server signature (Fixnum) is stored in `@sockets`, so
      # that it can be managed elsewhere, eg. `close_sockets()`.
      def setup_http_socket
        options = @settings[:client][:http_socket] || Hash.new
        options[:bind] ||= "127.0.0.1"
        options[:port] ||= 3031
        unless options[:enabled] == false
          @logger.debug("binding client http socket", :options => options)
          @sockets << EM::start_server(options[:bind], options[:port], HTTPSocket) do |socket|
            socket.logger = @logger
            socket.settings = @settings
            socket.transport = @transport
          end
        else
          @logger.info("client http socket disabled per configuration")
        end
      end

      # Setup the Sensu client sockets, JSON TCP & UDP, and HTTP.
      # Users can opt-out of using the HTTP socket via configuration.
      def setup_sockets
        setup_json_socket
        setup_http_socket
      end

      # Call a callback (Ruby block) when there are no longer check
      # executions in progress. This method is used when stopping the
      # Sensu client. The `retry_until_true` helper method is used to
      # check the condition every 0.5 seconds until `true` is
      # returned.
      #
      # @yield [] callback/block called when there are no check
      #   executions in progress.
      def complete_checks_in_progress
        @logger.info("completing checks in progress", :checks_in_progress => @checks_in_progress)
        retry_until_true do
          if @checks_in_progress.empty?
            yield
            true
          end
        end
      end

      # Create a check result intended for deregistering a client.
      # Client definitions may contain `:deregistration` configuration,
      # containing custom attributes and handler information. By
      # default, the deregistration check result sets the `:handler` to
      # `deregistration`. If the client provides its own `:deregistration`
      # configuration, it's deep merged with the defaults. The
      # check `:name`, `:output`, `:issued`, and `:executed` values
      # are always overridden to guard against an invalid definition.
      def deregister
        check = {:handler => "deregistration", :status => 1}
        if @settings[:client].has_key?(:deregistration)
          check = deep_merge(check, @settings[:client][:deregistration])
        end
        timestamp = Time.now.to_i
        overrides = {
          :name => "deregistration",
          :output => "client initiated deregistration",
          :issued => timestamp,
          :executed => timestamp
        }
        publish_check_result(check.merge(overrides))
      end

      # Close the Sensu client TCP and UDP sockets. This method
      # iterates through `@sockets`, which contains socket server
      # signatures (Fixnum) and connection objects. A signature
      # indicates a TCP socket server that needs to be stopped. A
      # connection object indicates a socket connection that needs to
      # be closed, eg. a UDP datagram socket.
      def close_sockets
        @logger.info("closing client sockets")
        @sockets.each do |socket|
          if socket.is_a?(Numeric)
            EM.stop_server(socket)
          else
            socket.close_connection
          end
        end
      end

      # Bootstrap the Sensu client, setting up client keepalives,
      # subscriptions, and standalone check executions. This method
      # sets the process/daemon `@state` to `:running`.
      def bootstrap
        setup_keepalives
        setup_subscriptions
        setup_standalone
        @state = :running
      end

      # Start the Sensu client process, setting up the client
      # transport connection, the sockets, and calling the
      # `bootstrap()` method.
      def start
        setup_transport do
          setup_sockets
          bootstrap
        end
      end

      # Pause the Sensu client process, unless it is being paused or
      # has already been paused. The process/daemon `@state` is first
      # set to `:pausing`, to indicate that it's in progress. All run
      # timers are cancelled, and the references are cleared. The
      # Sensu client will unsubscribe from all transport
      # subscriptions, then set the process/daemon `@state` to
      # `:paused`.
      def pause
        unless @state == :pausing || @state == :paused
          @state = :pausing
          @timers[:run].each do |timer|
            timer.cancel
          end
          @timers[:run].clear
          @transport.unsubscribe if @transport
          @state = :paused
        end
      end

      # Resume the Sensu client process if it is currently or will
      # soon be paused. The `retry_until_true` helper method is used
      # to determine if the process is paused and if the transport is
      # connected. If the conditions are met, `bootstrap()` will be
      # called and true is returned to stop `retry_until_true`.
      def resume
        retry_until_true(1) do
          if @state == :paused
            if @transport.connected?
              bootstrap
              true
            end
          end
        end
      end

      # Stop the Sensu client process, pausing it, completing check
      # executions in progress, closing the transport connection, and
      # exiting the process (exit 0). After pausing the process, the
      # process/daemon `@state` is set to `:stopping`.  Also sends
      # deregistration check result if configured to do so.
      def stop
        @logger.warn("stopping")
        last_state = @state
        pause
        if @settings[:client][:deregister] == true && last_state != :initializing
          deregister
        end
        @state = :stopping
        complete_checks_in_progress do
          close_sockets
          @transport.close if @transport
          super
        end
      end
    end
  end
end
