class C {
  void g() {
      
    a.f(
	//ERROR: match
	(
	 Object)this);
  }
}
