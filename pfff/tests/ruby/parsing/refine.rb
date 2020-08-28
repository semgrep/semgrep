# re-open Ruby's Time class
module RelativeTimeExtensions
  refine Time do
    def half_a_day_ago
      self - 43200
    end
  end
end

module MyModule
  class MyClass
    # Allow the refinement to be used
    using RelativeTimeExtensions

    def window
      Time.now.half_a_day_ago
    end
  end
end
