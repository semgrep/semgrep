test_nosem_func() // nosem
test_nosem_func() // nosem: rules.test-nosem
test_nosem_func() // NOSEM: rules.test-nosem

test_nosem_func(
    invalid_line // nosem: rules.test-nosem
)

// nosem
test_nosem_func()

// nosem: rules.test-nosem
test_nosem_func()

// NOSEM: rules.test-nosem
test_nosem_func()

test_nosem_func(
  // nosem: rules.test-nosem
  invalid_line
)

test_nosem_func()
