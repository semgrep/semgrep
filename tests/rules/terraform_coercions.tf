
resource "fakeresource" "fakername" {
    # ruleid: terraform-coercions 
    thing1 = 150
    # ruleid: terraform-coercions 
    thing2 = true
    # ruleid: terraform-coercions 
    thing3 = 1.50

    # ruleid: terraform-coercions 
    thing1_coerced = "150"
    # ruleid: terraform-coercions 
    thing2_coerced = "true"
    # ruleid: terraform-coercions 
    thing3_coerced = "1.50"
}
