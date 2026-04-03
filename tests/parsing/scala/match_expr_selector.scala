final class BatchProvider[A](name: String)(generateBatch: () => Fu[List[A]]):

  private var reserve = List.empty[A]

  def one: Fu[A] = workQueue:
    reserve.match
      case head :: tail =>
        reserve = tail
        fuccess(head)
      case Nil =>
        generateBatch().map:
          case head :: tail =>
            reserve = tail
            head
          case Nil => sys.error("couldn't generate batch")
