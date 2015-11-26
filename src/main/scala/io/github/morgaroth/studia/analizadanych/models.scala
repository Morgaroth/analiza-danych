package io.github.morgaroth.studia.analizadanych

case class Example(size: Int, procs: Int)

case class Result(procs: Int, time: Double)

case class Speedup(procs: Int, speedup: Double)

case class Efficiency(procs: Int, efficiency: Double)
