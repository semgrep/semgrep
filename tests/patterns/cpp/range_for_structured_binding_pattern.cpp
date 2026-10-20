#include <map>
#include <vector>

struct Point {
  int x;
  int y;
};

void probe(std::map<int, int>& values) {
  //ERROR: match
  for (auto [key, value] : values) {
    (void)key;
    (void)value;
  }
  //ERROR: match
  for (auto& [key, value] : values) {
    (void)key;
    (void)value;
  }
  //ERROR: match
  for (const auto& [key, value] : values) {
    (void)key;
    (void)value;
  }
  //ERROR: match
  for (auto&& [key, value] : values) {
    (void)key;
    (void)value;
  }
}

void probe_aggregate(std::vector<Point>& points) {
  //ERROR: match
  for (auto [x, y] : points) {
    (void)x;
    (void)y;
  }
}
