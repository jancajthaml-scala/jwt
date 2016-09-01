package com.github.jancajthaml.jwt

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object RangeBenchmark extends Bench[Double] {
  
  /* configuration */

  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min[Double],
    measurer
  )
  
  lazy val measurer = new Measurer.Default
  lazy val reporter = new LoggingReporter[Double]
  lazy val persistor = Persistor.None
  
  /* inputs */

  val sizes = Gen.range("size")(10, 1000, 100000)
  val maps = for (sz <- sizes) yield Map((0 until sz).toList map { a => a.toString -> a }: _*)
  
  /* tests */

  performance of "com.github.jancajthaml.jwt" in {
    measure method "jsondumps" in {
      using(maps) in jsondumps
    }
  }
  
}