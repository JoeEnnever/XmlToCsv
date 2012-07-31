package xmltocsv

import collection._
import compress.CompressFile
import swing.FileChooser.SelectionMode
import swing.{FlowPanel, FileChooser}
import java.io.{IOException, FileWriter, File}
import _root_.dsl.Dsl.Columns

object Main {

  def listXmlFiles(dir: File): Seq[File] = {
    if (dir.isDirectory) {
      val filter = fun2FileFiler(_.getAbsolutePath.endsWith("xml"))
      dir.listFiles(filter).toSeq
    }
    else
      Nil
  }

  def listXmlFiles(dir: String): Seq[File] = listXmlFiles(new File(dir))

  implicit def fun2FileFiler(f: (File) => Boolean) = new java.io.FileFilter {
    def accept(pathname: File): Boolean = f(pathname)
  }

  private def getXmlDir(arg: String) = {
    arg match {
      case "gui" => {
        val chooser = new FileChooser(new File("."))
        chooser.fileSelectionMode = SelectionMode.DirectoriesOnly
        chooser.title = "Select XML Directory"
        chooser.showDialog(new FlowPanel, "Select")
        chooser.selectedFile
      }
      case _ => new File(arg)
    }
  }

  private def getOutFile(title: String): File = {
    val chooser = new FileChooser(new File("."))
    chooser.fileSelectionMode = SelectionMode.FilesOnly
    chooser.title = title
    chooser.fileFilter = new SwingFileFilter("CSV Files", f => f.isDirectory || f.getPath.endsWith(".csv"))
    chooser.showDialog(new FlowPanel, "Select")
    chooser.selectedFile
  }

  private class SwingFileFilter(val getDescription: String, filter: (File) => Boolean) extends javax.swing.filechooser.FileFilter {
    def accept(f: File) = filter(f)
  }

  @throws(classOf[IOException])
  def run(xmlDir: File, outFile: File, columns: Columns) {
    println("Loading XML Files from %s\nWriting to %s".format(xmlDir, outFile.getAbsolutePath))
    val xmlFiles = listXmlFiles(xmlDir)
    if (xmlFiles.isEmpty) {
      Console.err.println("No XML files found in [" + xmlDir + "].\nExiting")
      sys.exit(1)
    }
    val output = new FileWriter(outFile, false)
    val registry = new MainRegistry(xmlFiles, columns)
    val csvData = registry.run()
    writeOutput(output, csvData)
  }

  def getFiles = {
    (getXmlDir("gui"), getOutFile("Select Output File"))
  }
  //TODO name better
  def getFilesTestably(args1: String, args2: String)= {
    (getXmlDir(args1), new File(args2))
  }

  def runWithColumns(columns: Columns) {
    val (xmlDir, outFile) = getFiles
    run(xmlDir, outFile, columns)
  }

  def main(args: Array[String]) {
    if(args.length >= 1 && args(0).equalsIgnoreCase("compress")){
      val file = getOutFile("Select File To Compress")
      val output = new CompressFile().compress(file)
      writeOutput(new FileWriter(file, false), output.iterator)
      return
    }

    try {
      val (xmlDir, outFile) = if (args.length > 1) getFilesTestably(args(0), args(1)) else getFiles
      val columnDefs = new Columns {
        def columns = Nil
      }
      run(xmlDir, outFile, columnDefs)
    } catch {
      case ioe: IOException => Console.err.println(ioe); sys.exit(1)
      case t => t.printStackTrace(); sys.exit(1)
    }
  }

  private def writeOutput(output: FileWriter, rows: Iterator[Iterable[String]]) {
    while(!rows.isEmpty){
      output write(rows.next().mkString(","))
      output write "\n"
      output flush()
    }
    output.close()
  }
  class MainRegistry(val files: Seq[File], val userColumns: Columns) extends ComponentRegistry {
    val fileSize = files.length
    val headerCreator = new FullText(files, "clinical_study")
    val csvCreator = new CreateCsvImpl
    val subListSelector = if (fileSize > 100) new SelectRandom(100) else new SelectAll
    val compressor = if(fileSize > 100) None else Some(new CompressCsv)
  }
}