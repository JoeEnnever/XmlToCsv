package xmltocsv.compress

import annotation.tailrec

/**
 *
 * <br>Date: 7/15/12
 * @author Joe Ennever
 */

trait CompressComponent {
  val compressor: Option[CompressCsv]
  class CompressCsv {
    private implicit def list2RemoveAll[T](xs: List[T]) = new RemoveAllList[T](xs)

    private class RemoveAllList[T](list: List[T]) {
      def removeAll(is: Iterable[Int]): List[T] = {
        doRemoveAll(list, is.toSet, 0, Nil).reverse
      }

      @tailrec
      private def doRemoveAll(from: List[T], is: Set[Int], i: Int, acc: List[T]): List[T] = {
        from match {
          case Nil => acc
          case x :: xs => {
            val newAcc = if (is contains i) acc else x :: acc
            doRemoveAll(xs, is, i + 1, newAcc)
          }
        }
      }
    }

    /**
     * The first row is taken to be the header, i.e. non-empty columns in it will be ignored
     * @param rows
     * @return
     */
    def compressCsv(rows: Iterable[IndexedSeq[String]]): Iterable[Seq[String]] = {
      val empties = findEmptyColumns(rows drop 1)
      rows.map(_.toList).map(_.removeAll(empties))
    }

    private def findEmptyColumns(rows: Iterable[IndexedSeq[String]]) = {
      for {i <- 0 until rows.head.size
           if rows.forall((row) => row(i).trim.isEmpty)}
      yield i
    }
  }

}