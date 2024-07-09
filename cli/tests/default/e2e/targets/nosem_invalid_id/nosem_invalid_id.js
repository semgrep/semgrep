/* Should match but not error */
test_nosem_func() // nosem: rules.test-nosem-invalid
/* No match reported here */
test_nosem_func() /* nosem: test-nosem */
/* Should match but not error */
test_nosem_func() /* nosem: */
/* No match reported here */
/* nosem: test-nosem */
test_nosem_func()
/* Should match but not error */
/* nosem: */
test_nosem_func()

