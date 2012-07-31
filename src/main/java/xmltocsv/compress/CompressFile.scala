package xmltocsv.compress

import java.io.{File =>JFile}
import io.Source

/**
 *
 * <br>Date: 7/15/12
 * @author Joe Ennever
 */

class CompressFile extends CompressComponent{
  val compressor = Some(new CompressCsv)
  private val comp = compressor.get
  def compress(file: JFile) = {
    val source = Source.fromFile(file)
    val lines = source.getLines().map(_.split(',').toIndexedSeq)
    val ret = comp.compressCsv(lines.toIterable)
    source.close()
    ret
  }
}
