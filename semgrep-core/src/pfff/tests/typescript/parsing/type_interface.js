interface PluralizedListProps<T> extends Foo<T> {
  items: T[];
  className?: string;
  //noItems?: React.ReactNode;
  //children(toRender: T): React.ReactNode;
}
