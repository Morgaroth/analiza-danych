package io.github.morgaroth.studia.analizadanych

import java.io.{File, FilenameFilter}

import org.sameersingh.scalaplot._

import scala.io.Source
import scala.language.{implicitConversions, reflectiveCalls}

/**
  * Created by mateusz on 24.11.15.
  */
trait BaseApp extends XYDataImplicits with XYChartImplicits {

  implicit def wrapToAverage(input: List[Double]): Object {def avg: Double} = new {
    def avg = input.sum / input.length
  }

  def normalizeSize(size: Int): String = size match {
    case 100 => "100"
    case 1000 => "1k"
    case 10000 => "10k"
    case 100000 => "100k"
    case 1000000 => "1kk"
    case 10000000 => "10kk"
    case 100000000 => "100kk"
    case 1000000000 => "1kkk"
    case oth => throw new RuntimeException("WTF size?")
  }

  def prefix: String = ""

  def filesFilter(name: String): Boolean = {
    name startsWith prefix
  }

  val defaultLineParser = (l: String) => l.toDouble

  val defaultFileParser = (f: File) => {
    Source.fromFile(f).getLines().toList.map(defaultLineParser)
  }

  def dataDir: String

  def loadData[T](fileParser: File => T): Map[String, T] = {
    val par = new File(dataDir)
    loadFiles(par).map { f =>
      f.getName -> fileParser(f)
    } toMap
  }

  def loadFiles(par: File): List[File] = {
    if (par.exists() && par.isDirectory) {
      par.listFiles(new FilenameFilter {
        override def accept(dir: File, name: String): Boolean = filesFilter(name.toLowerCase)
      }).toList
    } else List.empty
  }

  def normalizeData[T: Numeric](data: List[T], resultSize: Int = 10) = {
    val toRemove = data.size - resultSize
    val fromBegin = toRemove / 2
    data.sorted.slice(fromBegin, fromBegin + resultSize)
  }

  def minAggregate(values: List[Double]): Double = values.min

  def avgAggregate(values: List[Double]): Double = normalizeData(values).avg

  def generatePlot(
                    domain: List[Int],
                    datas: Iterable[(String, List[Double])],
                    xLabel: String, yLabel: String,
                    title: String, showLegends: Boolean = true): (String, XYChart) = {
    val xAx = new NumericAxis
    xAx.label = xLabel

    val yAx = new NumericAxis
    yAx.label = yLabel

    generatePlotBasic(domain, datas, title, showLegends, xAx, yAx)
  }

  def generateLogPlot(logX: Boolean, logY: Boolean,
                      domain: List[Int],
                      datas: Iterable[(String, List[Double])],
                      xLabel: String, yLabel: String,
                      title: String, showLegends: Boolean = true): (String, XYChart) = {
    val xAx = new NumericAxis
    if (logX) xAx.log
    xAx.label = xLabel

    val yAx = new NumericAxis
    if (logY) yAx.log
    yAx.label = yLabel

    generatePlotBasic(domain, datas, title, showLegends, xAx, yAx)
  }

  def generatePlotBasic(domain: List[Int], datas: Iterable[(String, List[Double])], title: String, showLegends: Boolean, xAx: NumericAxis, yAx: NumericAxis): (String, XYChart) = {
    val pointType: Some[PointType.Value] = Some(PointType.fullO)
    val pointSize: Option[Double] = None

    val chart: XYChart = plot(
      domain.map(_.toDouble) -> datas.map(d => Y(d._2, d._1, pt = pointType, ps = pointSize)),
      title = title, x = xAx, y = yAx,
      showLegend = showLegends,
      legendPosX = LegendPosX.Right
    )
    title -> chart
  }
}
