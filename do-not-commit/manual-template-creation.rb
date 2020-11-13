require 'erb'

class FaxHelper

  def to_fax
    html = File.open(path_to_template).read
    # ruleid: manual-template-creation
    template = ERB.new(html)
    template.result
  end

end


x = 42
# ruleid: manual-template-creation
template = ERB.new <<-EOF
  The value of x is: <%= x %>
EOF
puts template.result(binding)
