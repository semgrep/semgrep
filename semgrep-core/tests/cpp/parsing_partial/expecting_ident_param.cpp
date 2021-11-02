void foo () {
  
  TF_ASSIGN_OR_RETURN(
      ScalarIndexedArray* const folded_reshape_without_degenerate_dims,
      FoldReshapeOfGatherNoDegenerateDims(
          output_shape_without_degenerate_dims,
	  // tree-sitter thinks this is a function definition and skips
	  // the -> below, leading to a parameter 'as' with a template
	  // argument
          operand_without_degenerate_dims->as<ScalarIndexedConstantArray>()));

  if (folded_reshape_without_degenerate_dims == nullptr) {
    return nullptr;
  }

}

