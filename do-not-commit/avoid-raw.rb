# cf. https://github.com/rails/rails/blob/62d089a4ad0170320b851addf76f7f48a49d68d8/actionview/test/template/output_safety_helper_test.rb

# frozen_string_literal: true

require "abstract_unit"

class OutputSafetyHelperTest < ActionView::TestCase
  tests ActionView::Helpers::OutputSafetyHelper

  def setup
    @string = "hello"
  end

  test "raw returns the safe string" do
    # ruleid: avoid-raw
    result = raw(@string)
    assert_equal @string, result
    assert_predicate result, :html_safe?
  end

  test "raw handles nil values correctly" do
    # ruleid: avoid-raw
    assert_equal "", raw(nil)
  end

  test "safe_join should html_escape any items, including the separator, if they are not html_safe" do
    # ruleid: avoid-raw
    joined = safe_join([raw("<p>foo</p>"), "<p>bar</p>"], "<br />")
    assert_equal "<p>foo</p>&lt;br /&gt;&lt;p&gt;bar&lt;/p&gt;", joined

    # ruleid: avoid-raw
    joined = safe_join([raw("<p>foo</p>"), raw("<p>bar</p>")], raw("<br />"))
    assert_equal "<p>foo</p><br /><p>bar</p>", joined
  end

  test "safe_join should work recursively similarly to Array.join" do
    joined = safe_join(["a", ["b", "c"]], ":")
    assert_equal "a:b:c", joined

    joined = safe_join(['"a"', ["<b>", "<c>"]], " <br/> ")
    assert_equal "&quot;a&quot; &lt;br/&gt; &lt;b&gt; &lt;br/&gt; &lt;c&gt;", joined
  end

  test "safe_join should return the safe string separated by $, when second argument is not passed" do
    default_delimeter = $,

    begin
      $, = nil
      joined = safe_join(["a", "b"])
      assert_equal "ab", joined

      silence_warnings do
        $, = "|"
      end
      joined = safe_join(["a", "b"])
      assert_equal "a|b", joined
    ensure
      $, = default_delimeter
    end
  end
end
