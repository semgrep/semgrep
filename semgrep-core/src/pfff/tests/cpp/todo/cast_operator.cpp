class X {

  inline operator bool() const {
    return (mutex_ != NULL);
  }
};
