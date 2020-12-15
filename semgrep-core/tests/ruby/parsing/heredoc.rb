tr = <<~MSG
  NOT conditions will no longer behave as NOR in Rails 6.1.
  To continue using NOR conditions, NOT each condition individually
  (`#{
    opts.flat_map { |key, value|
      if value.is_a?(Hash) && value.size > 1
        value.map { |k, v| ".where.not(#{key.inspect} => { #{k.inspect} => ... })" }
      else
        ".where.not(#{key.inspect} => ...)"
      end
    }.join
  }`).
MSG
