#include <map>

void probe(std::map<int, int>& values, const char* source) {
  char buffer[8];
  //ERROR: match
  __builtin_strcpy(buffer, source);

  for (auto [key, value] : values) {
    (void)key;
    (void)value;
    //ERROR: match
    __builtin_strcpy(buffer, source);
  }
  for (auto& [key, value] : values) {
    (void)key;
    (void)value;
    //ERROR: match
    __builtin_strcpy(buffer, source);
  }
  for (const auto& [key, value] : values) {
    (void)key;
    (void)value;
    //ERROR: match
    __builtin_strcpy(buffer, source);
  }
  for (auto&& [key, value] : values) {
    (void)key;
    (void)value;
    //ERROR: match
    __builtin_strcpy(buffer, source);
  }

  auto& [first, second] = *values.begin();
  (void)first;
  (void)second;
  for (auto item : values) {
    (void)item;
    //ERROR: match
    __builtin_strcpy(buffer, source);
  }

  //ERROR: match
  __builtin_strcpy(buffer, source);
}
