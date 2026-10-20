#include <vector>

struct Point {
  int x;
  int y;
};

void probe(std::vector<Point>& points) {
  //ERROR: match
  for (auto [x, y] : points) {
    (void)x;
    (void)y;
  }
  for (const auto& [x, y] : points) {
    (void)x;
    (void)y;
  }
}
