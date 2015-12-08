package io.github.morgaroth.studia.analizadanych.ar.lab3

import io.github.morgaroth.studia.analizadanych._
import io.github.morgaroth.studia.analizadanych.Nodes._
import org.sameersingh.scalaplot.{XYChart, XYChartImplicits}

import scala.collection.immutable.Iterable
import scala.language.reflectiveCalls

/**
  * Created by mateusz on 24.11.15.
  */
object Apps extends App with BaseApp with GraphsSupport with XYChartImplicits {
  override def dataDir: String = "/home/mateusz/Dropbox/Studia/semIX/AR/zad3/dane/"


  override def directory: String = args.headOption.getOrElse("/home/mateusz/Dropbox/Studia/semIX/AR/zad3/wykresy/")


  override def postfix: String = ".csv"

  type Nodes = Int
  type Type = String
  type Time = Double

  def readData(data: Map[String, List[Time]], aggregator: List[Double] => Double): Iterable[(Type, Nodes, Time)] = {
    data.map {
      case (name, values) =>
        val types :: nodes :: Nil = name.stripSuffix(".csv").split("_").take(2).toList
        if (values.length <= 10) println(s"too low data (${values.length}) for problem $types by $nodes nodes")
        (s"V${types.toInt}", nodes.toInt, aggregator(values))
    }
  }

  override def defaultLineParser(l: String) = l.split(",")(1).toDouble

  val rawData: Map[String, List[Time]] = loadData(defaultFileParser).filter(_._2.nonEmpty)

  val gAVG: Iterable[(Type, Nodes, Time)] = readData(rawData, avgAggregate)
  val gMIN: Iterable[(Type, Nodes, Time)] = readData(rawData, minAggregate)

  analizeData(gAVG, "avg")
  analizeData(gMIN, "min")

  def analizeData(g: Iterable[(Type, Nodes, Time)], prefix: String): (String, XYChart) = {
    val byType: Map[Type, List[Result]] = g.groupBy(_._1).mapValues(_.map(x => Result(x._2, x._3)).toList.sortBy(_.nodes))
    println(byType)
    //    val single: Map[Type, Time] = byType.mapValues(_.find(_.nodes == 1).get.time)

    //    val speedups: Map[Type, List[Speedup]] = byType.map {
    //      case (size, times) => size -> times.map(x => Speedup(x.procs, single(size) / x.time))
    //    }
    //    val efficiencies: Map[Type, List[Efficiency]] = speedups.map {
    //      case (sizes, data) => sizes -> data.map(x => Efficiency(x.procs, x.speedup / x.procs))
    //    }
    val domain: List[Nodes] = byType.head._2.map(_.nodes).distinct.sorted
    println(s"used domain: $domain")
    val timesByNodes = byType.toList.sortBy(_._1).map {
      case (t, results) => t -> domain.map(x => results.find(_.nodes == x).get.time / 60000)
    }
    //    val speedupsData = speedups.toList.sortBy(_._1).map {
    //      case (size, results) => normalizeSize(size) -> domain.map(x => results.find(_.procs == x).get.speedup)
    //    }
    //    val efficienciesData = efficiencies.toList.sortBy(_._1).map {
    //      case (size, results) => normalizeSize(size) -> domain.map(x => results.find(_.procs == x).get.efficiency)
    //    }
    generatePlot(domain, timesByNodes, "nodes", "Time [min]", s"Time of each option per nodes $prefix").saveToPng()
    //    generatePlot(domain, speedupsData, "PROCS [n]", "", s"Speedup $prefix").saveToPng()
    //    generatePlot(domain, efficienciesData, "PROCS [n]", "", s"Efficiency $prefix").saveToPng()
    //    val timesRaw = g.groupBy(_._2).mapValues(_.map(x => x._1 -> x._3))
    //    val timesDomain = timesRaw.head._2.map(_._1).toList.distinct.sorted
    //    println(s"used timing domain: $timesDomain")
    //    val timesData = timesRaw.toList.sortBy(_._1).map {
    //      case (procs, times) => procs.toString -> timesDomain.map(d => times.find(_._1 == d).get._2)
    //    }
    //    generateLogPlot(logX = true, logY = true, timesDomain, timesData, "SIZE [n]", "time [s]", s"Time $prefix").saveToPng()
  }
}
