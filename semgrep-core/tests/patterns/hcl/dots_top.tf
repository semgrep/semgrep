#ERROR: match
variable "environment_tag" {
  description =  "Env Tag"
  type        = string
}

variable "name_tag" {
  description = "name_tag"
  type        = string
}

variable "ami" {
  description = "AMI to use for the instance."
  type        = string
}