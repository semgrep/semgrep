This is a copy of https://github.com/yaml/libyaml.git
revision 2c891fc7a770e8ba2fec34fc6b545c672beb37e6 (tag v0.2.5).

yaml.h has been modified to remove anonymous structs and unions, since ocaml-ctypes stub
generation needs to probe their sizes at compile-time.
