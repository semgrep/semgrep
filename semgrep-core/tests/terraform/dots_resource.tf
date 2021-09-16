#ERROR: match
resource "aws_ebs_volume" "web_host_storage" {
  availability_zone = "ap-southeast-2"
  encrypted         = true
  size = 1
  tags = {
    Name = "abcd-ebs"
  }
}

#OK
resource "aws_ebs_volume" "web_host_storage" {
  availability_zone = "ap-southeast-2"
  encrypted         = false
  size = 1
  tags = {
    Name = "abcd-ebs"
  }
}
