resource "aws_iam_policy" "policy" {
   name        = "test_policy"
   path        = "/"
   description = "My test policy"

   # Terraform's "jsonencode" function converts a
   # Terraform expression result to valid JSON syntax.
   policy = jsonencode({
     Version = "2012-10-17"
     Statement = [
       #ERROR: match
       {
         Action = "*"
         Effect = "Allow"
         Resource = "*"
       },
     ]
   })
 }

# works also now in any order! better than generic mode
resource "aws_iam_policy" "policy2" {
   name        = "test_policy"
   path        = "/"
   description = "My test policy"

   policy = jsonencode({
     Version = "2012-10-17"
     Statement = [
       #ERROR: match
       {
         Effect = "Allow"
         Resource = "*"
         Action = "*"
       },
     ]
   })
 }
 