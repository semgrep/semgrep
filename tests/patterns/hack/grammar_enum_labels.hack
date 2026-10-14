<?hh
// ERROR: match
consume(ConcreteEnum#Value, $value);
consume(ConcreteEnum#Other, $value);
consume(ConcreteEnum::Value, $value);
