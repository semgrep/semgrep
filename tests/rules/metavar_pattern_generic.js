// ok because of how generic mode works
foo(1234);

// ruleid: rule_template_id
foo(2);

// ruleid: rule_template_id
foo(1, 2, 3, 4);

foo(1, 3, 4);

// ruleid: rule_template_id
foo('2');
