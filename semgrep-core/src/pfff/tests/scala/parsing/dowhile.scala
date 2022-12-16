/**
  * Simple fixpoint solver.
  */
trait SimpleFixpointSolver extends LatticeSolver {

  /**
    * The constraint function for which the least fixpoint is to be computed.
    * @param x the input lattice element
    * @return the output lattice element
    */
  def fun(x: lattice.Element): lattice.Element

  /**
    * The basic Kleene fixpoint solver.
    */
  def analyze(): lattice.Element = {
    var x = lattice.bottom
    var t = x
    do {
      t = x
      x = fun(x)
    } while (x != t)
    x
  }
}
