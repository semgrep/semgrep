def options
       OptionParser.new do |o|
         o.set_summary_indent('  ')
         o.banner =    "Usage: #{@script_name} [OPTIONS] command"
         o.define_head "Rails plugin manager."

         o.separator ""
         o.separator "GENERAL OPTIONS"

         o.on("-r", "--root=DIR", String,
              "Set an explicit rails app directory.",
              "Default: #{@rails_root}") { |@rails_root| self.environment = RailsEnvironment.new(@rails_root) }
         o.on("-s", "--source=URL1,URL2", Array,
              "Use the specified plugin repositories instead of the defaults.") { |@sources|}

         # HERE $verbose
         o.on("-v", "--verbose", "Turn on verbose output.") { |$verbose| }
         o.on("-h", "--help", "Show this help message.") { puts o; exit }
      end
end
