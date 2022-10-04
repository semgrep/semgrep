locals {
  #ERROR: match
  data_science = "data-science"
}
resource "aws_s3_bucket" "athena_output_data_science" {
  #ERROR: match
  bucket        = local.data_science
}
