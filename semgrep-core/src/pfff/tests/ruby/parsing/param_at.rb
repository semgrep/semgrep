def match(str)
       accum = []
       @string_left = str
       @matched_count = 0

       @specs.each_with_index do |spec,@i|
         @last_spec_tried = spec
         @last_match_tried = spec.match(@string_left)
         break unless @last_match_tried
         @matched_count += 1

         accum << spec.conversion

         @string_left = @last_match_tried.post_match
         break if @string_left.empty?
       end
       return accum.compact
     end
