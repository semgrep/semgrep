// ERROR:
select foo(5)

// ERROR:
select foo(1, 5)

select foo(1, 5, 0)
select foo()
select bar(5)