// ERROR:
select foo(bar(42))
// ERROR:
select foo(42)

select bar(42, foo())
select foo(bar())