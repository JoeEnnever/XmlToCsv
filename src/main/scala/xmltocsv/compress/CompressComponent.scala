package xmltocsv.compress

import annotation.tailrec
import java.io.File
import io.Source

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
     * @return
     */
    def compressCsv(file: File): Iterator[IndexedSeq[String]] = {
      val source1 = Source.fromFile(file)
      val source2 = Source.fromFile(file)
      try {
        val rows = source1.getLines().map(_.split(',').toIndexedSeq)
        val size = rows.next().size
        val empties = findEmptyColumns(rows, size)
        source2.getLines().map(_.split(',').toIndexedSeq).map(_.toList).map(_.removeAll(empties)).map(_.toIndexedSeq)
      } finally {
        source1.close()
        source2.close()
      }
    }

    private def findEmptyColumns(rows: Iterator[IndexedSeq[String]], size: Int) = {
      val present = new Array[Boolean](size)
      for {
        row <- rows
        i <- 0 until size
      } {
        if (row.isDefinedAt(i) && !row(i).trim.isEmpty){
          present(i) = true
        }
      }
      for {
        i <- 0 until size
        if !present(i)
      } yield i
    }

    var count = 0;
  }

}