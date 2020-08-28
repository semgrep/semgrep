<?hh

type Foo = int;


type SearchRbtRuleSignature = (
  SearchRbtRuleVertical,
  SearchRbtRuleRelation,
  bool
);

// trailing comma
type SearchRbtRuleSignature = (
  SearchRbtRuleVertical,
  SearchRbtRuleRelation,
  bool,
);
