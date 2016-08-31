package com.github.jancajthaml.jwt

import javax.crypto.Mac

object Main extends App {

  val secretKey: String = "secret"
  val charset: String = "utf-8"

  def decode(token: String): Map[String, Any] = {
    val chunks: Array[String] = token.split('.')
    val header = jsonloads(base64decode(chunks(0)))

    header.get("typ") match {
      case Some("JWT") => {}
      case x => throw new DeserializationException(s"Invalid type for JWT decoding ${x}")
    }

    val signature = chunks(2).getBytes(charset)
    val calculatedSignature: Array[Byte] = (getAlg(header.get("alg"), secretKey.getBytes(charset)) match {
      case None => throw new DeserializationException(s"Unsupported algorithm")
      case Some(x) => base64encode(
        x.doFinal((chunks(0) + ('.' +: chunks(1))).getBytes(charset))
      ).getBytes(charset)
    })

    (calculatedSignature.length == signature.length match {
      case true => if ((signature zip calculatedSignature).foldLeft (0) {(r, ab) => r + (ab._1 ^ ab._2)} == 0) true
      case _ => new DeserializationException(s"Invalid token, signature does not match")
    })

    jsonloads(base64decode(chunks(1)))
  }

  def encode(body: Map[String, Any], alg: String) = {
    val header: String = base64encode(jsondumps(Map("typ" -> "JWT", "alg" -> alg)))
    val payload: String = base64encode(jsondumps(body))
    (header + ('.' +: payload) + ('.' +: (
      getAlg(Option(alg), secretKey.getBytes(charset)) match {
        case None => throw new DeserializationException(s"Unsupported algorithm")
        case Some(x) => base64encode(x.doFinal((header + ('.' +: payload)).getBytes(charset)))
      }
    )))
  }

  ////

  def time[A](a: => A, n:Int) = {
    var times = List[Long]()
      for (_ <- 1 to n) {
      val now = System.nanoTime
      val res = a
      times :::= List(System.nanoTime - now)
    }
    val result = times.sum / n
    println((result / 1e6) + "ms")
    result
  }

  val sampleJWT: String = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeFONFh7HgQ"
  val sampleJSON: String = jsondumps(Map("x" -> "y"))

  println("[i] encoding performance")
  var a = 0
  for (a <- 1 to 10) {
    time({
      encode(Map("foo" -> "bar", "XXX" -> "XXX"), "HS256")
    }, 5000)
  }

  println("[i] decoding performance")
  var b = 0
  for (b <- 1 to 10) {
    time({
      decode(sampleJWT)
    }, 5000)
  }

  println("[i] dumps performance")
  var c = 0
  for (c <- 1 to 10) {
    time({
      jsondumps(Map("x" -> "y"))
    }, 5000)
  }

  println("[i] loads performance")
  var d = 0
  for (d <- 1 to 10) {
    time({
      jsonloads(sampleJSON)
    }, 5000)
  }

  println("[i] sanity check")
  println(
    decode(
      encode(
        Map(
          "foo" -> "fooValue",
          "bar" -> "barValue"
        ),
        "HS256"
      )
    )
  )

}
