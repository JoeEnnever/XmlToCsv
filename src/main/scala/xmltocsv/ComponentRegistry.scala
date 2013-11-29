package xmltocsv

import compress.CompressComponent
import gencsv.CreateCsvComponent
import genheader.{SubListSelectorComponent, CreateHeaderComponent}
import collection.parallel.{ParSeq, Splitter}
import collection.parallel.immutable.ParIterable
import collection.parallel.mutable.ParArray

/**
 *
 * <br>Date: 7/15/12
 * @author Joe Ennever
 */

trait ComponentRegistry extends CreateHeaderComponent
    with CreateCsvComponent
    with CompressComponent
    with SubListSelectorComponent
    with DslBinder {
  type HeaderType = IndexedSeq[String]
  type RowsType = Splitter[Map[String, String]]
  type ReturnType = (HeaderType, Iterator[IndexedSeq[String]])
  def run(): ReturnType = {
    val header = headerCreator.header.toIndexedSeq
    val nodeList = csvCreator.createCsv(header)
    combine(header, nodeList)
  }

  def combine(header: HeaderType, nodeList: RowsType): ReturnType = {
    val headerMap = header.indices.map(i => i -> header(i)).toMap
    val nodeTransformed: Splitter[IndexedSeq[String]] = {
      for (node <- nodeList) yield {
        header.indices.map { i =>
          if(node.contains(headerMap(i))) {
            node(headerMap(i)).filter(_ != ',')
          } else {
            ""
          }
        }
      }
    }.asInstanceOf[Splitter[IndexedSeq[String]]]
    (header, nodeTransformed)
  }
}