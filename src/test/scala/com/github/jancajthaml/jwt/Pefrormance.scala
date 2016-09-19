package com.github.jancajthaml.jwt

import org.scalameter.api.{Bench, Gen, exec}
import scala.util.{Try,Success,Failure}

object Regression extends Bench.OfflineReport {

  val secret = "secret"
  val times = Gen.range("times")(0, 10000, 2000)
  val token: String = encode(Map(), "HS256", "secret") match {
    case Success(x) => x
    case Failure(f) => ""
  }

  performance of "com.github.jancajthaml.jwt" in {
    measure method "encode" in {
      using(times) config (
        exec.minWarmupRuns -> 2,
        exec.maxWarmupRuns -> 5,
        exec.benchRuns -> 5,
        exec.independentSamples -> 1
      ) in { sz => { (0 to sz).foreach { x => { encode(Map(), "HS256", secret) } } } }
    }
    measure method "decode" in {
      using(times) config (
        exec.minWarmupRuns -> 2,
        exec.maxWarmupRuns -> 5,
        exec.benchRuns -> 5,
        exec.independentSamples -> 1
      ) in { sz => { (0 to sz).foreach { x => { decode(token, secret) } } } }
    }
  }
  
}
