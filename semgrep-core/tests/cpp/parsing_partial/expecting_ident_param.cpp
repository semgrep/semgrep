void foo () {
  
  TF_ASSIGN_OR_RETURN(
      ScalarIndexedArray* const folded_reshape_without_degenerate_dims,
      FoldReshapeOfGatherNoDegenerateDims(
          output_shape_without_degenerate_dims,
          operand_without_degenerate_dims->as<ScalarIndexedConstantArray>()));

  if (folded_reshape_without_degenerate_dims == nullptr) {
    return nullptr;
  }

}

