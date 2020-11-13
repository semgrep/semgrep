def bad_nested_attributes_bypass
    # ruleid: nested-attributes-bypass
    accepts_nested_attributes_for allow_destroy: false

    # ruleid: nested-attributes-bypass   
    accepts_nested_attributes_for :avatar, :book, allow_destroy:false

    # ruleid: nested-attributes-bypass   
    accepts_nested_attributes_for :avatar, :book, allow_destroy:false, :name
end

def ok_nested_attributes_bypass
    has_one :avatar
    # ok
    accepts_nested_attributes_for :avatar, allow_destroy: true
end


