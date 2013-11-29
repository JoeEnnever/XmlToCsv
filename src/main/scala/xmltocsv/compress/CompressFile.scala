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
    comp.compressCsv(file)
  }
}
