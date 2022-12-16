#ifndef _FOO_H_
#define _FOO_H_

#include <stdio.h>

class Foo {


protected:
  char *pt;
  int lg;

public:
  Foo() { pt = NULL; lg=0; }
  Foo(const char *);
  Foo(const Foo &);
  ~Foo();

  void afficher() const;

  Foo & operator=(const Foo &);
  Foo & operator+=(const Foo &);
  Foo & operator-=(const Foo &);
  char & operator[](int);

protected:
  friend int Foo_strcmp(const Foo &c1,const Foo &c2);

public:

  friend bool operator<(const Foo &, const Foo &);
  friend bool operator>(const Foo &,const Foo &);
  friend bool operator==(const Foo &,const Foo &);
  friend Foo operator+(const Foo &c1,const Foo &c2);
  friend Foo operator-(const Foo &c1,const Foo &c2);

};

#endif
