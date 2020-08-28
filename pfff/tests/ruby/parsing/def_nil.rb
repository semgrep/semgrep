def test_nil_instance_eval_cvar # [ruby-dev:24103]
    def nil.test_binding
      binding
    end
    bb = eval("nil.instance_eval \"binding\"", nil.test_binding)
    assert_raise(NameError) { eval("@@a", bb) }
    class << nil
      remove_method :test_binding
    end
end
