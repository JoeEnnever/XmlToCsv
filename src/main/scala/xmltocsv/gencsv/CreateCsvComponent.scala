package xmltocsv.gencsv

import java.io.{File => JFile}
import xml._
import xmltocsv.DslBinder
import scala.collection.mutable
import xml.Text
import collection.parallel.Splitter

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
    def createCsv(header: Seq[String]): Splitter[Map[String, String]]
  }

  object CreateCsv {
    implicit class CsvableNode(node: NodeSeq) {
      def trimDigits(str: String): (String, Int) = {
        val index = str indexOf '#'
        if (index < 0)
          (str, 0)
        else {
          val tuple = str splitAt index
          (tuple._1, tuple._2.drop(1).toInt)
        }
      }

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
      def getValues: Map[String, String] = {
        def getValues(nodes: Elem, curPrefix: String, map: mutable.Map[String, String]): Map[String, String] = {
          var count = 0
          var priorLabel: String = ""
          for (node <- nodes.child) {
            val curLabel = node.label
            node match {
              case n: Node if n.label == "#PCDATA" => map += curPrefix -> n.text.filter(_ != '\n')
              case _ => {
                val num =
                  if (priorLabel == curLabel) {
                    count += 1
                    count
                  } else {
                    priorLabel = curLabel
                    count = 0
                    count
                  }
                  val prefix  = curPrefix + "|" + curLabel + "#" + num
                  node match {
                    case x: Elem => getValues(x, prefix, map)
                    case t: Text => map += prefix -> t.text.filter(_ != '\n')
                  }
              }
            }
          }
          map.toMap
        }
        getValues(node.asInstanceOf[Elem], node(0).label+"#0", mutable.Map.empty)
      }
    }

  }

  class CreateCsvImpl extends CreateCsv {
    def createCsv(header: Seq[String]) = {
      import CreateCsv.CsvableNode
      val nodeStream = files.par.iterator.map{f => println("loading from file [%s]".format(f.getName)); XML.loadFile(f)}
      for {
        node <- nodeStream
        userDefined = userColumns.visitNode(node)
      } yield node.getValues ++ userDefined
    }.asInstanceOf[Splitter[Map[String,String]]]
  }

}
