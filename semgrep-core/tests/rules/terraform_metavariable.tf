
locals {
    x = 5
}

# ruleid: terraform-metavariable
resource "foo" "bar" {
    qux = local.x
}