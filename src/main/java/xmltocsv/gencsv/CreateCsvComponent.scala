package xmltocsv.gencsv

import java.io.{File => JFile}
import xml.{Elem, NodeSeq, XML}
import xmltocsv.DslBinder

/**
 *
 * <br>Date: 7/15/12
 * @author Joe Ennever
 */
trait CreateCsvComponent {
  this: DslBinder =>
  val csvCreator: CreateCsv
  val files: Seq[JFile]
  trait CreateCsv {
    def createCsv(header: Seq[String]): Iterator[IndexedSeq[String]]
  }

  private object CreateCsv {

    class CsvableNode(val node: NodeSeq) {
      def getValue(location: String): String = {
        val nodeSeq = location.split('|').filter(_ != "").drop(1)
        var curNode = node
        for (nodeName <- nodeSeq) {
          val tuple = trimDigits(nodeName)
          curNode = (curNode \ tuple._1)
          if (curNode.isDefinedAt(tuple._2)) {
            curNode = curNode(tuple._2)
          } else {
            return ""
          }
        }
        if (curNode(0).child.exists(_.isInstanceOf[Elem]))
          ""
        else
          curNode(0).text.trim.filterNot(_ == '\n')
      }

    }

    def trimDigits(str: String): (String, Int) = {
      val index = str indexOf '#'
      if (index < 0)
        (str, 0)
      else {
        val tuple = str splitAt index
        (tuple._1, tuple._2.drop(1).toInt)
      }
    }

    implicit def node2Csvable(node: NodeSeq) = new CsvableNode(node)

  }

  class CreateCsvImpl extends CreateCsv {
    def createCsv(header: Seq[String]) = {
      import CreateCsv._
      val loadFileFun = (f: JFile) => {
        (f, XML.loadFile(f))
      }
      val nodeStream = files.iterator.map(loadFileFun)
      val indexedHeader = header.toIndexedSeq
      val nodeList =
        for {
          (file, node) <- nodeStream
        } yield {
          println("Loading from file [%s]".format(file.getName))
          val columnDefs = userColumns
          val userDefined = columnDefs.visitNode(node)
          for {col <- indexedHeader} yield {
            if (userDefined contains (col)){
              userDefined(col)
            } else {
              node.getValue(col).filter(_ != ',')
            }
          }
        }
      nodeList
    }
  }

}