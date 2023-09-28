void range_base_loop(std::set<int *> &set) {
  for (int *p : set) {
    // `p` is re-initialized on every iteration of the
    // range-based loop. There is no path from source
    // to sink with the same pointer
    // ok: source-sink-range
    sink(p);
    source(p);
  }
}
