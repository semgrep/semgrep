# intentional typo
#ruleid: http-client
variable "environment_ta" {
  description =  "Env Tag"
  type        = string
}

variable "name_tag" {
  description = "name_tag"
  type        = string
}

# if have typo on environment_tag above
#ruleid: http-client
variable "ami" {
  description = "AMI to use for the instance."
  type        = string
}