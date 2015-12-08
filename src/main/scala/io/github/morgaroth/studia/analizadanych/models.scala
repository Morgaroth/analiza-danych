package io.github.morgaroth.studia.analizadanych

object Procs {

  case class Example(size: Int, procs: Int)

  case class Result(procs: Int, time: Double)

  case class Speedup(procs: Int, speedup: Double)

  case class Efficiency(procs: Int, efficiency: Double)

}


object Nodes {

  case class Example(mode: String, nodes: Int)

  case class Result(nodes: Int, time: Double)

  case class Speedup(nodes: Int, speedup: Double)

  case class Efficiency(nodes: Int, efficiency: Double)

}