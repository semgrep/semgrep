# intentional typo
#ruleid: match
variable "environment_ta" {
  description =  "Env Tag"
  type        = string
}

variable "name_tag" {
  description = "name_tag"
  type        = string
}

#ruleid: match, if have typo on environment_tag above
variable "ami" {
  description = "AMI to use for the instance."
  type        = string
}