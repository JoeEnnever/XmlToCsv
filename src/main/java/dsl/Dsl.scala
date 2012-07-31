package dsl

import xml.{XML, NodeSeq, Elem}
import dsl.Dsl.Columns

//  create column "Num Locations" {
//    count of("clinical_trial", "location")
//  }
//  create column "Num US Locations" {
//    count of("clinical_trial", "location") where("facility", "address", "country") is ("United States" | "US" | "USA")
//  }
object Dsl {
  def count = new Count

  implicit def col2CondBuilder(col: ColumnData) = new CondBuilder(col, Nil)
  implicit def condBuilder2Cond(builder: CondBuilder) = builder.toConditional
  implicit def string2Cond(s: String) = new Conditional(s == _)

  def create = new HeaderBuilder

  //    private val bar =
  //    create column ("Num Locations") {
  //      count of ("clinical_trial", "location") where ("facility", "address", "country") is "United States"
  //    }
  /*
    for header use {
      all files
      10 percent of files
      100 random files
      header from file "Foo.csv"
    }

   */
  private[dsl] sealed abstract class ColumnData{
    type T
    def accumulate: (T, NodeSeq) => T
    def baseValue: T
    def visit(root: NodeSeq): T = {
      root.foldLeft(baseValue)(accumulate)
    }
  }
  private[dsl] case class Count() extends ColumnData{
    type T = Int
    def accumulate = (count, nodeSeq) => count + 1
    def baseValue = 0
  }
  private[dsl] case class ConditionalColumn(base: ColumnData, baseFields: Seq[String], matchFields: Seq[String], cond: Conditional) extends ColumnData{
    type T = base.T
    def accumulate = base.accumulate
    def baseValue = base.baseValue
    override def visit(root: NodeSeq): T = {
      explodeElem(root, baseFields) match {
        case None => baseValue
        case Some(nodes) => {
          if(matchFields.isEmpty) super.visit(nodes)
          else {
            explodeElem(nodes, matchFields) match {
              case None => baseValue
              case Some(matchNodes) => {
                super.visit(matchNodes.filter( (node) => cond(node.text)))
              }
            }
          }
        }
      }
    }
  }

  private[dsl] case class Header(name: String, data: ColumnData)

  private[dsl] class Conditional(cond: (String) => Boolean) extends ((String) => Boolean) {
    def apply(s: String) = cond(s)
    def or(other: Conditional) = new Conditional(s => if (apply(s)) true else other(s))
    def and(other: Conditional) = new Conditional(s => if (apply(s)) other(s) else false)
    def |(other: Conditional) = or(other)
    def &(other: Conditional) = and(other)
  }

  private[dsl] class HeaderBuilder {
    def column(name: String) = new {
      def having(data: => ColumnData) = new Header(name, data)
    }
  }

  private[dsl] class CondBuilder(col: ColumnData, baseFields: Seq[String]) {
    def of(baseFields: String*) = new CondBuilder(col, baseFields){
      def where(matchFields: String*) = new {
        def is(matcher: Conditional) = ConditionalColumn(col, baseFields, matchFields, matcher)
      }
    }
    def toConditional = ConditionalColumn(col, baseFields, Nil, new Conditional((s)=>true))
  }

  abstract class Columns {
    def columns: List[Header]
    def columnNames = columns.map(_.name)

    def visitNode(root: NodeSeq): Map[String, String] = {
      columns.map((header) => (header.name, header.data.visit(root).toString)).toMap
    }
   }
  def explodeElem(node: NodeSeq, fields: Seq[String]): Option[NodeSeq] = {
    if (fields.isEmpty) Some(node)
    else {
      node match {
        case el: Elem if el.label == fields(0) => explodeElem(node, fields drop 1)
        case _ => {
          node \ fields(0) match {
            case child: NodeSeq if !(child.isEmpty) => explodeElem(child, fields drop 1)
            case _ => None
          }
        }
      }
    }
  }
}

object Foo {
  def main(args: Array[String]) {
    import Dsl._
    val columns = new Columns {
      def columns = List(
        create column "Foo" having {
          count of ("clinical_study", "location") where ("facility", "address", "country") is ("United States"|"Zanzibar")
        }
      )
    }
    val exampleXml = XML.loadFile("C:\\Users\\Tiina\\workspace\\xmlToCsv\\src\\main\\resources\\examples\\NCT00000900.xml")
    println(columns.visitNode(exampleXml))

  }
}