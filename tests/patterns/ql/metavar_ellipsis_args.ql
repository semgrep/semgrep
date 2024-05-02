// ERROR:
select foo(3)
// ERROR:
select foo(1, 3, 1)
// ERROR:
select foo(1, 2, 3, 1, 2)

select foo(1, 3)
select foo(1, 3, 2)

select foo(1, 2, 4)