variable "v" {
  type    = string
  default = "150"
}

# ruleid: terraform-mvar-regex-interpolated-var
resource "foo" "bar" {
    field = "${var.v}"
}

# ruleid: terraform-mvar-regex-interpolated-var
resource "foo" "bar" {
    field = "150"
}

resource "foo" "bar" {
    field = "random"
}

variable "v2" {
  type    = number
  default = 150
}

# ruleid: terraform-mvar-regex-interpolated-var
resource "foo" "bar" {
    field = "${var.v2}"
}