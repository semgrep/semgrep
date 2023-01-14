#ERROR: match
resource "aws_iam_policy" "policy" {
   name        = "test_policy"
   path        = "/"
   description = "My test policy"

   # Terraform's "jsonencode" function converts a
   # Terraform expression result to valid JSON syntax.
   policy = jsonencode({
     Version = "2012-10-17"
     Statement = [
       {
         #ruleid: no-iam-admin-privileges
         Action = "*"
         Resource = "*"
       },
     ]
   })
 }
 