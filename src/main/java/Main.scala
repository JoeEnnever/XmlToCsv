import collection.immutable.TreeMap
import collection.mutable.HashMap
import java.io.{FileFilter, File}
import xml._

/**
 *
 * <br>Date: 6/12/12
 * @author Joe Ennever
 */
object Main extends App{
 def histogram(nodes: Seq[Elem]): TreeMap[String, Int] = {
   val collect = HashMap[String, Int]()
   for(node <- nodes){
     val hist = _histogram (node, "")
     mergeMap(collect, hist)
   }
   val builder = TreeMap.newBuilder[String, Int]
   for(entry <- collect){
     builder += entry
   }
   builder.result()
 }

 private def _histogram(nodes: Node, prefix: String): Map[String, Int] = {
   nodes match {
     case text: Text => Map()
     case node: Elem => {
       val subPrefix = prefix+"|"+node.label
       val collect = HashMap[String, Int]()
       node.child.filter(_.isInstanceOf[Elem]).foreach((node) => {
         val newPrefix = subPrefix+"|"+node.label
         collect.put(newPrefix, collect.get(newPrefix).getOrElse(0)+1)
         val subMap = _histogram(node, subPrefix)
         mergeMap(collect, subMap)
       })
       collect.toMap
     }
   }
 }

  def mergeMap[K, V](mutable: HashMap[K, V], immutable: Map[K, V])(implicit comp: Ordering[V]) {
    for (entry <- immutable) {
      val key = entry._1
      val value = entry._2
      val bigger = if (mutable contains key) {
        comp.max(mutable.get(key).get, value)
      } else {
        value
      }
      mutable.put(key, bigger)
    }
  }


  def listXmlFiles(dir: String): Seq[File] = {
    val f = new File(dir)
    f.listFiles(new FileFilter{
      def accept(pathName: File) = pathName.getAbsolutePath.endsWith("xml")
    }).toSeq
  }

  def csvHeader(histogram: Map[String, Int], head: String): List[String] = {
    val startingMap = histogram + (("|"+head, 1))
    _csvHeader(startingMap, List(), head, "|"+head)
  }

  private def _csvHeader(histogram: Map[String, Int], appendTo: List[String], prefix: String, key: String): List[String] = {
    val count = histogram.get(key).getOrElse(0)
    val filtered = histogram.filter((e) => e._1.startsWith(key) && e._1 != key)
    var collect = appendTo
    for( i <- 1 to count;
         entry <- filtered){
      val newPrefix = prefix + i
      collect = newPrefix :: appendTo
      val newNewPrefix = (newPrefix + "|" + entry._1.drop(newPrefix.length)).replaceAllLiterally("||", "|")
      val newKey = (key + "|" + entry._1.drop(newPrefix.length)).replaceAllLiterally("||", "|")
      println("newKey [%s] ".format(newNewPrefix, newKey))
      collect = _csvHeader(histogram, appendTo, newNewPrefix, newKey) ::: collect
    }
    collect
  }

  def debug[T](t: T): T = {println(t); t}

  class CsvableNode(val node: Elem){
    def toCsv(hist: Map[String, Int], appendTo: StringBuilder, prefix: String): String = {
      ""
    }
  }

  implicit def node2Csvable(node: Elem) = new CsvableNode(node)

  val nodes = listXmlFiles(args(0)).map(XML.loadFile(_))

  if(args.length < 1){
    println("Usage: scala Main xmlDir")
    System.exit(1)
  }
  val hist = histogram(nodes)
  println(csvHeader(histogram(nodes), "clinical_study"))
  
}