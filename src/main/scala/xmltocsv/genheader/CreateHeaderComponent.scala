package xmltocsv.genheader

import java.io.File
import scala.xml.{XML, Text, Node, Elem}
import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.io.Source
import xmltocsv.{DslBinder, AlphaNumericComparator}

/**
 *
 * <br>Date: 7/15/12
 * @author Joe Ennever
 */

trait CreateHeaderComponent {
  this: DslBinder with SubListSelectorComponent =>
  val headerCreator: CreateHeader

  class FullText(private val files: Seq[File],
                 private val headNode: String) extends CreateHeader {
    lazy val header = {
      val nodes = subListSelector.selectSubList(files.toList).iterator.map(
      {f => println("Parsing header from file [%s]".format(f.getName)); XML.loadFile(f)})
      import CreateHeader._
      userColumns.columnNames ++ csvHeader(histogram(nodes), headNode).toIndexedSeq.sorted(AlphaNumericComparator)
    }
    override val customColumns = userColumns.columnNames
  }

  class LoadedFromFile(private val file: File) extends CreateHeader {
    lazy val header = {
      userColumns.columnNames  ++ Source.fromFile(file).takeWhile(_ != '\n').mkString("").split(",").toSeq
    }
  }


  class Predefined(predefinedHeader: Seq[String]) extends CreateHeader {
    val header = userColumns.columnNames ++ predefinedHeader
  }
}

trait FilteredHeader extends CreateHeader {
  def filter: (String) => Boolean
  abstract override def header = {
    super.header.filter(filter)
  }
}

trait CreateHeader {
  def header: Seq[String]
  def customColumns: Seq[String] = Nil
}

private[genheader] object CreateHeader {
  def histogram(nodes: Iterator[Elem]): TreeMap[String, Int] = {
    val collect = mutable.HashMap[String, Int]()
    for (node <- nodes) {
      val hist = _histogram(node, "")
      mergeMap(collect, hist)
    }
    TreeMap(collect.toList: _*)
  }

  def _histogram(nodes: Node, prefix: String): Map[String, Int] = {
    nodes match {
      case text: Text => Map()
      case node: Elem => {
        val subPrefix = prefix + "|" + node.label
        val collect: mutable.HashMap[String, Int] = mutable.HashMap()
        node.child.filter(_.isInstanceOf[Elem]).foreach((node) => {
          val newPrefix = subPrefix + "|" + node.label
          collect.put(newPrefix, collect.get(newPrefix).getOrElse(0) + 1)
          val subMap = _histogram(node, subPrefix)
          mergeMap(collect, subMap)
        })
        collect.toMap
      }
    }
  }

  def mergeMap[K, V](into: mutable.HashMap[K, V], immutable: Map[K, V])(implicit comp: Ordering[V]) {
    for (entry <- immutable) {
      val key = entry._1
      val value = entry._2
      val bigger = comp.max(into.get(key).getOrElse(value), value)
      into.put(key, bigger)
    }
  }

  def csvHeader(histogram: Map[String, Int], head: String): List[String] = {
    val startingMap = histogram + (("|"+head) -> 1)
    _csvHeader(startingMap, List(), head, "|" + head).filterNot(_ == head + 1)
  }

  private def _csvHeader(histogram: Map[String, Int], appendTo: List[String], prefix: String, key: String): List[String] = {
    val count = histogram.get(key).getOrElse(0)
    val filtered = histogram.filter((e) => e._1.startsWith(key) && e._1 != key)
    var collect = appendTo
    for (i <- 1 to count) {
      val newPrefix = prefix + "#" + (i - 1)
      collect = newPrefix :: collect
      for (entry <- filtered) {
        val lastPipe = entry._1.lastIndexOf('|')
        val ending = entry._1.splitAt(lastPipe)._2.drop(1)
        val newNewPrefix = (newPrefix + "|" + ending).replaceAllLiterally("||", "|")
        val newKey = (key + "|" + ending).replaceAllLiterally("||", "|")
        collect = _csvHeader(histogram, collect, newNewPrefix, newKey)
      }
    }
    collect
  }
}