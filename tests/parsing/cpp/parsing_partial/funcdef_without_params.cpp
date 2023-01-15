TEST_P(OpConverter_FP32_Test, ConvertEinsum) {

  // tree-sitter thinks this is a function definition, and
  // we then can't find parameters in it
  std::vector<TestParams> params{

#if !IS_TRT_VERSION_GE(8, 0, 0, 0)
          TestParams{"cebfad,fageb->abcdg"},
#endif
  };
  }
}

