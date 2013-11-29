package dsl

import xml.{NodeSeq, Elem}
import xmltocsv.genheader.CreateHeader
import annotation.tailrec
import collection.parallel.mutable
import collection.mutable.{ArrayBuffer, ListBuffer}

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

  def column = new ColumnFilterBuilder

  def filter = new {
    def columns(builder: => ColumnFilter) = builder.filter
  }

  private[dsl] class ColumnFilterBuilder {
    def is(names: String*) = new ColumnFilter{
      val r = """#\d+""".r
      val joined = names.mkString("|")
      override def filter = s => r.replaceAllIn(s,"") == joined
    }
    def contains(str: String) = new ColumnFilter(){
      override def filter = s => s.contains(str)
    }
  }


  private[dsl] abstract class ColumnFilter {
    def filter: (String) => Boolean

    def at_most(depth: Int): ColumnFilter = {
      val sup = this
      val r = """.*?#(\d+)(.*)""".r
      new ColumnFilter {
        override def filter = s => sup.filter(s) && {
          var cur = s
          var ret = false
          var break = false
          while(!break && !ret) {
            cur = cur match {
              case r(digit, rest) => if (digit.toInt > depth) { println("break"); break = true; "" } else rest
              case _ => { ret = true; "" }
            }
          }
          ret
        }
      }
    }

    def or(o: ColumnFilter) = ||(o)
    def and(o: ColumnFilter) = &&(o)

    def ||(other: ColumnFilter): ColumnFilter = {
      val thisFilter = this.filter
      val otherFilter = other.filter
      new ColumnFilter {
        override def filter = str => {
          thisFilter(str) || otherFilter(str)
        }
      }
    }
    def &&(other: ColumnFilter): ColumnFilter = {
      val thisFilter = this.filter
      val otherFilter = other.filter
      new ColumnFilter {
        override def filter = str => {
          thisFilter(str) || otherFilter(str)
        }
      }
    }
  }

  private[dsl] sealed abstract class ColumnData {
    type T
    def accumulate: (T, NodeSeq) => T
    def baseValue: T
    def visit(root: NodeSeq): T = {
      root.foldLeft(baseValue)(accumulate)
    }
  }
  private[dsl] case class Count() extends ColumnData {
    type T = Int
    def accumulate = (count, nodeSeq) => count + 1
    def baseValue = 0
  }
  private[dsl] case class ConditionalColumn(
      base: ColumnData,
      baseFields: Seq[String],
      matchFields: Seq[String],
      cond: Conditional) extends ColumnData{
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
    private var _name: String = _
    private var _data: ColumnData = _
    def build = Header(cnn(_name), cnn(_data))
    def column(name: String) = new {
      def having(data: => ColumnData): Unit = {
        _name = name
        _data = data
      }
    }
    private def cnn[T](t: T) = {
      if (t == null) {
        throw new NullPointerException
      }
      t
    }
  }

  private[dsl] class CondBuilder(col: ColumnData, baseFields: Seq[String]) {
    def of(baseFields: String*) = new CondBuilder(col, baseFields){
      def where(matchFields: String*) = new {
        def is(matcher: Conditional) = ConditionalColumn(col, baseFields, matchFields, matcher)
        def is_not(matcher: Conditional) = ConditionalColumn(col, baseFields, matchFields, new Conditional(s => !matcher(s)))
        def is_not_null = is(new Conditional(s => s != null && s != ""))
      }
    }
    def toConditional = ConditionalColumn(col, baseFields, Nil, new Conditional((s)=>true))
  }

  class OutputConfiguration {
    def create = {
      val header = new HeaderBuilder
      builders += header
      header
    }

    val builders = new ArrayBuffer[HeaderBuilder]()
    lazy val columns = builders.map(_.build).toSeq
    var includeFilter: Option[ColumnFilter] = None
    var excludeFilter: Option[ColumnFilter] = None

    var compress = false

    def columnNames = columns.map(_.name)

    def visitNode(root: NodeSeq): Map[String, String] = {
      columns.map((header) => (header.name, header.data.visit(root).toString)).toMap
    }

    def applyFilter(headerCreator: CreateHeader): CreateHeader = {
      if (includeFilter.isDefined) {
        includeFilter.get.filter //make sure its defined
        new CreateHeader {
          def header: Seq[String] = {
            headerCreator.customColumns ++: headerCreator.header.filter(includeFilter.get.filter)
          }
        }
      } else {
        headerCreator
      }
    }

    def filter = new {
      def columns(builder: => ColumnFilter) = OutputConfiguration.this.includeFilter = Some(builder)
    }

    def include_columns_where(columnFilter: => ColumnFilter) {
      OutputConfiguration.this.includeFilter = Some(columnFilter)
    }

    def exclude_columns_where(columnFilter: => ColumnFilter) {
      OutputConfiguration.this.excludeFilter = Some(columnFilter)
    }
  }

  @tailrec
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
    new OutputConfiguration {

      // define the extra columns to appear in the CSV
      create column "Name" having {
        count of ("clinical_study", "location") where ("facility", "address", "country") is ("United States"|"Zanzibar")
      }

      // Define which columns to include from the XML file
      include_columns_where {
        (column is ("clinical_study", "foo")) ||
        (column contains "location" at_most 10) ||
        (column contains "foo")
      }

      xmltocsv.Main.run_with(this)
    }
  }
}
