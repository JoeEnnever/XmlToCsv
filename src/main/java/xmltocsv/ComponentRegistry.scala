package xmltocsv

import compress.CompressComponent
import gencsv.CreateCsvComponent
import genheader.{SubListSelectorComponent, CreateHeaderComponent}

/**
 *
 * <br>Date: 7/15/12
 * @author Joe Ennever
 */

trait ComponentRegistry extends CreateHeaderComponent with CreateCsvComponent with CompressComponent with SubListSelectorComponent with DslBinder{
  def run(): Iterator[Seq[String]] = {
    val header = headerCreator.header
    val nodeList = csvCreator.createCsv(header)
    val totalCsv = Iterator(header.toIndexedSeq) ++ nodeList
    if (compressor.isDefined){
      compressor.get.compressCsv(totalCsv.toIterable).iterator
    } else {
      totalCsv
    }
  }
}