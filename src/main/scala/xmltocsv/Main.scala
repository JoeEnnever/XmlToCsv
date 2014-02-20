package xmltocsv

import scala.collection._
import compress.CompressFile
import scala.swing.FileChooser.SelectionMode
import scala.swing.{FlowPanel, FileChooser}
import java.io.{IOException, FileWriter, File}
import dsl.Dsl.OutputConfiguration

object Main {

  def listXmlFiles(dir: File): Seq[File] = {
    if (dir.isDirectory) {
      dir.listFiles.filter(_.getPath.endsWith("xml")).toSeq
    }
    else
      Nil
  }

  def listXmlFiles(dir: String): Seq[File] = listXmlFiles(new File(dir))

  private def getXmlDir(arg: String): File = {
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

  def run(xmlDir: File, outFile: File, config: Option[OutputConfiguration] = None) {
    val xmlFiles = listXmlFiles(xmlDir)
    if (xmlFiles.isEmpty) {
      Console.err.println("No XML files found in [" + xmlDir + "].\nExiting")
      sys.exit(1)
    }
    println("Loading XML Files from %s\nWriting to %s".format(xmlDir, outFile.getAbsolutePath))
    val output = new FileWriter(outFile, false)
    val registry = new MainRegistry(xmlFiles, config)
    val csvData = registry.run()
    writeOutput(output, csvData._1, csvData._2)
  }

  def getFiles = {
    (getXmlDir("gui"), getOutFile("Select Output File"))
  }
  //TODO name better
  def getFilesTestably(args1: String, args2: String)= {
    (getXmlDir(args1), new File(args2))
  }

  def run_with(config: OutputConfiguration) {
    val (xmlDir, outFile) = getFiles
    run(xmlDir, outFile, Some(config))
  }

  implicit class StrIgnoreCase(str: String) {
    def ignoreCase = this
    def unapply(other: String) = {
      if(str.equalsIgnoreCase(other)) Some(other)
      else None
    }
  }
  def main(args: Array[String]) {
    try {
      val compress = "compress".ignoreCase
      args match {
        case Array(compress(x), _*) => {
          val file = getOutFile("Select File To Compress")
          val outFile = getOutFile("Select Output File")
          val output = new CompressFile().compress(file)
          val head = output.next()
          writeOutput(new FileWriter(outFile, false), head, output)
        }
        case Array(args1, args2, _*) => {
          val (xmlDir, outFile) = getFilesTestably(args1, args2)
          run(xmlDir, outFile)
        }
        case _ => {
          val (xmlDir, outFile) = getFiles
          run(xmlDir, outFile)
        }
      }
    } catch {
      case ioe: IOException => ioe.printStackTrace(); Console.err.println(ioe); sys.exit(1)
      case t: Throwable => t.printStackTrace(); sys.exit(1)
    }
  }

  private def writeOutput(output: FileWriter, header: Iterable[String], rows: Iterator[Iterable[String]]) {
    try {
      output write (header.mkString(","))
      output write "\n"
      output flush()
      rows.foreach {
        row =>
          output write (row.mkString(","))
          output write "\n"
          output flush()
      }
    } finally {
      output.close()
    }
  }
  class MainRegistry(val files: Seq[File], configOpt: Option[OutputConfiguration]) extends ComponentRegistry {
    val fileSize = files.length
    val userColumns = configOpt.getOrElse(new OutputConfiguration)
    val headerCreator = userColumns.applyFilter(new FullText(files, "clinical_study"))
    val csvCreator = new CreateCsvImpl
    val subListSelector = new SelectAll
    val compressor = if (userColumns.compress) None else Some(new CompressCsv)
  }
}
