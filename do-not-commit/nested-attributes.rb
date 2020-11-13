class bad_use_nested_attrs
  has_one :author
  has_many :pages

  # ruleid: nested-attributes
  accepts_nested_attributes_for :author, :pages
end

class ok_use_nested_attrs
  has_one :author
  has_many :pages
end
