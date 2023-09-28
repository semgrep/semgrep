
namespace xla {

void BFloat16Propagation::AdjustCalledComputationRoot(HloInstruction* hlo) {
  auto adjust_computation = [this, hlo](HloComputation* computation,
                                        HloInstruction* output) {

  switch (hlo->opcode()) {
    case HloOpcode::kConditional:
      for (auto* branch : hlo->branch_computations()) {
        adjust_computation(branch, hlo);
      }
      break;
    default:
      break;
  }
}

}
