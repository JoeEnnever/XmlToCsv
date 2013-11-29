package xmltocsv.genheader

trait SubListSelectorComponent {
  val subListSelector: SubListSelector
  trait SubListSelector {
    def selectSubList[T](list: List[T]): List[T]
  }

  class SelectAll extends SubListSelector {
    def selectSubList[T](list: List[T]) = { println("select all"); list }
  }

  class SelectRandom(amt: Int) extends SubListSelector {
    def selectSubList[T](list: List[T]) = {
      val length = list.length
      val selection = new Array[Int](amt).map(x => (scala.math.random * length).toInt).toSet
      recSelect(list, 0, selection, Nil)
    }

  }

  class SelectPercentage(percent: Double) extends SubListSelector {
    def selectSubList[T](list: List[T]) = {
      val length = list.length
      val selectAmt = (length * percent).toInt
      val selection = new Array[Int](selectAmt).map(x => (scala.math.random * length).toInt).toSet
      recSelect(list, 0, selection, Nil)
    }
  }
  private def recSelect[T](list: List[T], i: Int, select: Set[Int], acc: List[T]): List[T] = {
    list match {
      case Nil => acc
      case x :: xs => {
        val newAcc = if (select contains i) x :: acc else acc
        recSelect(xs, i + 1, select, newAcc)
      }
    }
  }

}