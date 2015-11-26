package io.github.morgaroth.studia.analizadanych

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.XYChart
import org.sameersingh.scalaplot.jfreegraph.JFGraphPlotter

import scala.language.implicitConversions
import scala.util.Try

trait Graphs {

  def dir: String

  def ensureDirExists() = {
    new File(dir).mkdirs()
  }

  def normalizeName(string: String): String = {
    string.replaceAll(" ", "_").replaceAll("""\\n""", "_")
  }

  val savePng = (data: (String, XYChart)) => {
    ensureDirExists()
    output(PNG(s"$dir/", normalizeName(data._1)), data._2)
    data
  }

  val showJFGraph = (data: (String, XYChart)) => {
    ensureDirExists()
    val plotter = new JFGraphPlotter(data._2)
    plotter.gui()
    data
  }
  val saveJFPDF = (data: (String, XYChart)) => {
    ensureDirExists()
    val plotter = new JFGraphPlotter(data._2)
    plotter.pdf(s"$dir/", normalizeName(data._1))
    data
  }

  val savePDF = (data: (String, XYChart)) => {
    ensureDirExists()
    output(PDF(s"$dir/", normalizeName(data._1)), data._2)
    data
  }

  val showGui = (data: (String, XYChart)) => {
    output(GUI, data._2)
    data
  }

  val saveSVG = (data: (String, XYChart)) => {
    ensureDirExists()
    Try {
      val writer: PrintWriter = new PrintWriter(Files.createFile(Paths.get(s"$dir/${normalizeName(data._1)}.svg")).toFile)
      writer.write(output(SVG, data._2))
      writer.close()
    }
    data
  }
}

class SavableChart(data: (String, XYChart), directory: String) {
  val renderer = new Graphs {
    override def dir: String = directory
  }

  def showGui() = {
    renderer.showGui(data)
  }

  def showJFGraph() = {
    renderer.showJFGraph(data)
  }

  def saveJFPDF() = {
    renderer.saveJFPDF(data)
  }

  def saveToPng() = {
    renderer.savePng(data)
  }

  def saveToSVG(): (String, XYChart) = {
    renderer.saveSVG(data)
  }

  def saveToPDF() = {
    renderer.savePDF(data)
  }

}

trait GraphsSupport {
  def directory: String

  implicit def wrapAsSavable(data: (String, XYChart)): SavableChart = new SavableChart(data, directory)
}
