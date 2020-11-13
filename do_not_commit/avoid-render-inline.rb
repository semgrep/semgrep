# cf. https://github.com/rails/rails/blob/939fe523126198d43ecedeacc05dd7fdb1eae3d9/actionpack/test/controller/action_pack_assertions_test.rb


# frozen_string_literal: true

require "abstract_unit"
require "controller/fake_controllers"

class ActionPackAssertionsController < ActionController::Base
  def nothing() head :ok end

  # ok: avoid-render-inline
  def hello_xml_world() render template: "test/hello_xml_world"; end

  def assign_this
    @howdy = "ho"
    # ruleid: avoid-render-inline
    render inline: "Mr. Henke"
  end

  def render_based_on_parameters
    # ok: avoid-render-inline
    render plain: "Mr. #{params[:name]}"
  end

  def render_url
    # ok: avoid-render-inline
    render html: "<div>#{url_for(action: 'flash_me', only_path: true)}</div>"
  end

  def render_text_with_custom_content_type
    # ok: avoid-render-inline
    render body: "Hello!", content_type: Mime[:rss]
  end

  def session_stuffing
    session["xmas"] = "turkey"
    # ok: avoid-render-inline
    render text: "ho ho ho"
  end

  def raise_exception_on_get
    raise "get" if request.get?
    # ok: avoid-render-inline
    render text: "request method: #{request.env['REQUEST_METHOD']}"
  end

  def raise_exception_on_post
    raise "post" if request.post?
    # ok: avoid-render-inline
    render plain: "request method: #{request.env['REQUEST_METHOD']}"
  end

  def render_file_absolute_path
    # ok: avoid-render-inline
    render file: File.expand_path("../../README.rdoc", __dir__)
  end

  def render_file_relative_path
    # ok: avoid-render-inline
    render file: "README.rdoc"
  end
end

# Used to test that assert_response includes the exception message
# in the failure message when an action raises and assert_response
# is expecting something other than an error.
class AssertResponseWithUnexpectedErrorController < ActionController::Base
  def index
    raise "FAIL"
  end

  def show
    # ok: avoid-render-inline
    render plain: "Boom", status: 500
  end
end

module Admin
  class InnerModuleController < ActionController::Base
    def index
      head :ok
    end

    def redirect_to_index
      redirect_to admin_inner_module_path
    end

    def redirect_to_absolute_controller
      redirect_to controller: "/content"
    end

    def redirect_to_fellow_controller
      redirect_to controller: "user"
    end

    def redirect_to_top_level_named_route
      redirect_to top_level_url(id: "foo")
    end
  end
end

class ApiOnlyController < ActionController::API
  def nothing
    head :ok
  end

  def redirect_to_new_route
    redirect_to new_route_url
  end
end

class ActionPackAssertionsControllerTest < ActionController::TestCase
  def test_render_file_absolute_path
    get :render_file_absolute_path
    assert_match(/\A= Action Pack/, @response.body)
  end

  def test_render_file_relative_path
    get :render_file_relative_path
    assert_match(/\A= Action Pack/, @response.body)
  end

  def test_get_request
    assert_raise(RuntimeError) { get :raise_exception_on_get }
    get :raise_exception_on_post
    assert_equal "request method: GET", @response.body
  end

  def test_post_request
    assert_raise(RuntimeError) { post :raise_exception_on_post }
    post :raise_exception_on_get
    assert_equal "request method: POST", @response.body
  end

  def test_get_post_request_switch
    post :raise_exception_on_get
    assert_equal "request method: POST", @response.body
    get :raise_exception_on_post
    assert_equal "request method: GET", @response.body
    post :raise_exception_on_get
    assert_equal "request method: POST", @response.body
    get :raise_exception_on_post
    assert_equal "request method: GET", @response.body
  end

  def test_string_constraint
    with_routing do |set|
      set.draw do
        get "photos", to: "action_pack_assertions#nothing", constraints: { subdomain: "admin" }
      end
    end
  end

  def test_with_routing_works_with_api_only_controllers
    @controller = ApiOnlyController.new

    with_routing do |set|
      set.draw do
        get "new_route", to: "api_only#nothing"
        get "redirect_to_new_route", to: "api_only#redirect_to_new_route"
      end

      process :redirect_to_new_route
      assert_redirected_to "http://test.host/new_route"
    end
  end

  def test_assert_redirect_to_named_route_failure
    with_routing do |set|
      set.draw do
        get "route_one", to: "action_pack_assertions#nothing", as: :route_one
        get "route_two", to: "action_pack_assertions#nothing", id: "two", as: :route_two

        ActiveSupport::Deprecation.silence do
          get ":controller/:action"
        end
      end
      process :redirect_to_named_route
      assert_raise(ActiveSupport::TestCase::Assertion) do
        assert_redirected_to "http://test.host/route_two"
      end
      assert_raise(ActiveSupport::TestCase::Assertion) do
        assert_redirected_to %r(^http://test.host/route_two)
      end
      assert_raise(ActiveSupport::TestCase::Assertion) do
        assert_redirected_to controller: "action_pack_assertions", action: "nothing", id: "two"
      end
      assert_raise(ActiveSupport::TestCase::Assertion) do
        assert_redirected_to route_two_url
      end
    end
  end