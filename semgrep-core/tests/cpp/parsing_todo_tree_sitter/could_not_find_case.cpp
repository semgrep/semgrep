
// Returns Python buffer protocol's type string from TFRT's dtype.
static const char* ToPythonStructFormat(DType dtype_kind) {
  // Reference: https://docs.python.org/3/library/struct.html

  switch (dtype_kind) {
    case DType::Invalid:
      throw std::runtime_error("Invalid dtype.");
    default:
      throw std::runtime_error("Unimplemented.");
  }
}
