package io.github.morgaroth.studia.analizadanych.ar.lab2

import io.github.morgaroth.studia.analizadanych._
import io.github.morgaroth.studia.analizadanych.Procs._
import org.sameersingh.scalaplot.{XYChart, XYChartImplicits}

import scala.collection.immutable.Iterable
import scala.language.reflectiveCalls

/**
  * Created by mateusz on 24.11.15.
  */
object Apps extends App with BaseApp with GraphsSupport with XYChartImplicits {
  override def dataDir: String = "/home/mateusz/Dropbox/Studia/semIX/AR/zad2/data/"


  override def directory: String = args.headOption.getOrElse("/home/mateusz/Dropbox/Studia/semIX/AR/zad2/wykresy/")

  override def prefix: String = "100"

  type Procs = Int
  type Size = Int
  type Time = Double

  def readData(data: Map[String, List[Time]], aggregator: List[Double] => Double): Iterable[(Procs, Procs, Time)] = {
    data.map {
      case (name, values) =>
        val size :: procs :: Nil = name.stripSuffix(".log").split("_").take(2).toList
        if (values.length <= 10) println(s"too low data (${values.length}) for problem $size by $procs processors")
        (size.toInt, procs.toInt, aggregator(values))
    }
  }

  val rawData: Map[String, List[Time]] = loadData(defaultFileParser).filter(_._2.nonEmpty)
  val gAVG: Iterable[(Size, Procs, Time)] = readData(rawData, avgAggregate)
  val gMIN: Iterable[(Size, Procs, Time)] = readData(rawData, minAggregate)

  analizeData(gAVG, "avg")
  analizeData(gMIN, "min")

  def analizeData(g: Iterable[(Size, Procs, Time)], prefix: String): (String, XYChart) = {
    val bySize: Map[Size, List[Result]] = g.groupBy(_._1).mapValues(_.map(x => Result(x._2, x._3)).toList.sortBy(_.procs))
    val single: Map[Size, Time] = bySize.mapValues(_.find(_.procs == 1).get.time)
    val speedups: Map[Size, List[Speedup]] = bySize.map {
      case (size, times) => size -> times.map(x => Speedup(x.procs, single(size) / x.time))
    }
    val efficiencies: Map[Size, List[Efficiency]] = speedups.map {
      case (sizes, data) => sizes -> data.map(x => Efficiency(x.procs, x.speedup / x.procs))
    }
    val domain: List[Size] = bySize.head._2.map(_.procs).distinct.sorted
    println(s"used domain: $domain")
    val speedupsData = speedups.toList.sortBy(_._1).map {
      case (size, results) => normalizeSize(size) -> domain.map(x => results.find(_.procs == x).get.speedup)
    }
    val efficienciesData = efficiencies.toList.sortBy(_._1).map {
      case (size, results) => normalizeSize(size) -> domain.map(x => results.find(_.procs == x).get.efficiency)
    }
    generatePlot(domain, speedupsData, "PROCS [n]", "", s"Speedup $prefix").saveToPng()
    generatePlot(domain, efficienciesData, "PROCS [n]", "", s"Efficiency $prefix").saveToPng()
    val timesRaw = g.groupBy(_._2).mapValues(_.map(x => x._1 -> x._3))
    val timesDomain = timesRaw.head._2.map(_._1).toList.distinct.sorted
    println(s"used timing domain: $timesDomain")
    val timesData = timesRaw.toList.sortBy(_._1).map {
      case (procs, times) => procs.toString -> timesDomain.map(d => times.find(_._1 == d).get._2)
    }
    generateLogPlot(logX = true, logY = true, timesDomain, timesData, "SIZE [n]", "time [s]", s"Time $prefix").saveToPng()
  }
}
