# was not parsing in original diamondback-ruby implem, I had to change
# the priority of the ternary operator.

    def after(command = nil, &block)
      @after ||= block ? block : command
    end
