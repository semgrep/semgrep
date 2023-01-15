const isNonIdealStateProps = (object: any): object is NonIdealStateProps => {
  if (object == null) {
    return false;
  }
  return object.icon != null && object.title != null;
};
