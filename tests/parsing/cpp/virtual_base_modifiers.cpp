struct Base { virtual ~Base() = 0; virtual operator bool() const = 0; };
struct VirtualOnly : virtual Base {};
struct AccessFirst : public virtual Base {};
struct VirtualFirst : virtual protected Base {};
