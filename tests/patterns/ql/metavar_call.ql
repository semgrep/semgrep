// ERROR:
select foo(1, 2)
// ERROR:
select bar(1, 2)

select foo()
select foo(2, 1)
select bar(2, 1)