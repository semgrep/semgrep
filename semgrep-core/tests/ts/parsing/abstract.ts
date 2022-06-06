interface EdgeType<T> {
  cursor: Cursor;
  node: T;
}
type EdgeClassType<T> = abstract new () => EdgeType<T>;
