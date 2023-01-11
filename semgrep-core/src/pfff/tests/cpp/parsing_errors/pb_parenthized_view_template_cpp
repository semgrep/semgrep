
// the parenthized view must not consider the ',' in the lexical cast
// as a separator of arguments for foo(), otherwise we will get
// [lexical_cast<string;  ...] and we will get some exceptions
// when trying to regard the matching > of the >
x = foo(lexical_cast<string,int>(i), 3);
