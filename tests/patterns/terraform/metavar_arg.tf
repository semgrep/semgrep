resource "aws_ebs_volume" "web_host_storage" {

    #ERROR:
    a = foo(1,2)

    #ERROR:
    b = foo(a_very_long_constant_name,
           2)

    #ERROR:
    c = foo (unsafe(), #indeed
             2)

    #ERROR:
    d = foo(bar(1,3), 2)

    e = foo(2,1)
}