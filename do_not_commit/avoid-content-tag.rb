# cf. https://apidock.com/rails/ActionView/Helpers/TagHelper/content_tag

# ruleid: avoid-content-tag
content_tag(:p, "Hello world!")
 # => <p>Hello world!</p>

# ruleid: avoid-content-tag
content_tag(:div, content_tag(:p, "Hello world!"), class: "strong")
 # => <div class="strong"><p>Hello world!</p></div>

# ruleid: avoid-content-tag
content_tag(:div, "Hello world!", class: ["strong", "highlight"])
 # => <div class="strong highlight">Hello world!</div>

# ruleid: avoid-content-tag
content_tag("select", options, multiple: true)
 # => <select multiple="multiple">...options...</select>

# cf. https://stackoverflow.com/a/4205709
module InputHelper
  def editable_input(label,name)
    # ruleid: avoid-content-tag
    content_tag :div, :class => "field" do
      # ruleid: avoid-content-tag
      content_tag(:label,label) + # Note the + in this line
      text_field_tag(name,'', :class => 'medium new_value')
    end
  end
end
