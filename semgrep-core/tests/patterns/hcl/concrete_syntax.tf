resource "aws_ebs_volume" "web_host_storage" {
 #ERROR:
 a = foo(1,2)

 #ERROR:
 b = foo(1,
         2)

 #ERROR:
 c = foo (1, # comment
          2)

 d = foo(2,1)
}
