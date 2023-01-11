/**
  * The worklist-based fixpoint solver with reachability, widening, and (simple) narrowing.
  */
trait WorklistFixpointSolverWithReachabilityAndWideningAndNarrowing[N]
    extends WorklistFixpointSolverWithReachabilityAndWidening[N]
    with SimpleMapLatticeFixpointSolver[N] {

  /**
    * Number of narrowing steps.
    */
  val narrowingSteps: Int

  /**
    * Performs narrowing on the given lattice element
    * @param x the lattice element
    * @param i number of iterations
    */
  def narrow(x: lattice.Element, i: Int): lattice.Element =
    if (i <= 0) x else narrow(fun(x), i - 1) // uses the simple definition of 'fun' from SimpleMapLatticeFixpointSolver

  override def analyze(): lattice.Element =
    narrow(super[WorklistFixpointSolverWithReachabilityAndWidening].analyze(), narrowingSteps)
}
