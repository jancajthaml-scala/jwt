package com.github.jancajthaml.jwt

import scala.util.{Try,Success,Failure}

object encode extends ((Map[String, Any], String, String) => scala.util.Try[String]) {

  def apply(body: Map[String, Any], alg: String, secret: String): scala.util.Try[String] = {
    //for rainbow table prevention, shuffle values in header from time to time
    val header: String = base64encode(jsondumps(Map("alg" -> alg, "typ" -> "JWT")))
    val payload: String = base64encode(jsondumps(body))
    val signature = getAlg(Option(alg), secret.getBytes("utf-8")) match {
      case None => throw new Exception(s"Unsupported algorithm")
      case Some(x) => base64encode(x.doFinal((header + ('.' +: payload)).getBytes("utf-8")))
    }

    Success(header + ('.' +: payload) + ('.' +: (signature)))
  }
}

object decode extends ((String, String) => scala.util.Try[Map[String, Any]]) {

  def apply(token: String, secret: String): scala.util.Try[Map[String, Any]] = {
    val chunks: Array[String] = token.split('.')
    val header = jsonloads(base64decode(chunks(0)))
    header.get("typ") match {
      case Some("JWT") => {}
      case x => throw new Exception(s"Invalid type for JWT decoding ${x}")
    }
    val signature = chunks(2).getBytes("utf-8")
    val calculatedSignature: Array[Byte] = (getAlg(header.get("alg"), secret.getBytes("utf-8")) match {
      case None => throw new Exception(s"Unsupported algorithm")
      case Some(x) => base64encode(
        x.doFinal((chunks(0) + ('.' +: chunks(1))).getBytes("utf-8"))
      ).getBytes("utf-8")
    })
    (calculatedSignature.length == signature.length match {
      case true => if ((signature zip calculatedSignature).foldLeft (0) {(r, ab) => r + (ab._1 ^ ab._2)} == 0) true
      case _ => throw new Exception(s"Invalid token, signature does not match")
    })
    Try(jsonloads(base64decode(chunks(1))))
  }
}
/*
object Main extends App {

  val secretKey: String = "secretKey"
  val algorithm: String = "HS256"

  ////

  def time[A](a: => A, n:Int) = {
    var times = List[Long]()
      for (_ <- 1 to n) {
      val now = System.nanoTime
      val res = a
      times :::= List(System.nanoTime - now)
    }
    val result = times.sum / n
    println("%1.0f ys ~> %1.4f ms".format((result / 1e3), (result / 1e6)))
    result
  }

  val sampleJWT: String = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeFONFh7HgQ"
  val sampleMap: Map[String, Any] = Map(
    "stringKey" -> "this is a string with spaces",
    "floatKey" -> Float.MaxValue,
    "boolKey" -> true,
    "nullKey" -> null
  )
  val sampleJSON: String = jsondumps(sampleMap)

  println("[i] encoding time (average)")
  var a = 0
  for (a <- 1 to 10) {
    time({
      encode(
        sampleMap,
        algorithm,
        secretKey
      )
    }, 10000)
  }

  println("[i] decoding time (average)")
  var b = 0
  for (b <- 1 to 10) {
    time({
      decode(
        sampleJWT,
        secretKey
      )
    }, 10000)
  }

  println("[i] dumps time (average)")
  var c = 0
  for (c <- 1 to 10) {
    time({
      jsondumps(sampleMap)
    }, 10000)
  }

  println("[i] loads time (average)")
  var d = 0
  for (d <- 1 to 10) {
    time({
      jsonloads(sampleJSON)
    }, 10000)
  }

  println("[i] sanity check")
  encode(
    sampleMap,
    algorithm,
    secretKey
  ) match {
    case Success(token) => {
      println(s"original payload: $sampleMap")
      println(s"JWT: $token")  
      decode(token, secretKey) match {
      case Success(decoded) => println(s"decoded payload: $decoded")
      case Failure(f) => println(f)
    }
    }
    case Failure(f) => println(f)
  }

  
  /*

  def f[T](v: T) = v match {
    case _: Number => "Number"
    case _: Boolean => "Boolean"
    case _: String => "String"
    case null => "null"
    case _ => "Unknown"
  }

  for ((k,v) <- decoded) {
    println(s"|$k| -> |$v| [${f(v)}]")
  }
*/

}
*/