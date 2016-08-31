package com.github.jancajthaml.jwt

import com.github.jancajthaml.json.JSON.{parse => loads}
import javax.crypto.Mac

object Main extends App {

  val secretKey: String = "secret"
  val charset: String = "utf-8"

  def dumps(value: Map[String, Any]): String = {
    "{" + value.map(x => {("\"" + x._1 + "\":\"" + x._2 + "\"")}).mkString("",", ","") + "}"
  }

  def decode(token: String): Map[String, Any] = {
    val chunks: Array[String] = token.split('.')
    var alg: Option[String] = None

    ("""\"(.*?)\":\s*\"(.*?)\",?""".r).findAllIn(base64decode(chunks(0))).matchData.foreach({m => {
      m.group(1) match {
        case "typ" => m.group(2) match {
          case "JWT" => {}
          case x => throw new DeserializationException(s"Invalid type for JWT decoding ${x}")
        }
        case "alg" => {
          alg = Some(m.group(2))
        }
      }
      }
    })

    val mac: Mac = getAlg(alg, secretKey.getBytes(charset)) match {
      case None => throw new DeserializationException(s"Unsupported algorithm")
      case Some(x) => x
    }

    val signature = chunks(2).getBytes(charset)
    val calculatedSignature: Array[Byte] = base64encode(
      mac.doFinal((chunks(0) + ('.' +: chunks(1))).getBytes(charset))
    ).getBytes(charset)

    (calculatedSignature.length == signature.length match {
      case true => if ((signature zip calculatedSignature).foldLeft (0) {(r, ab) => r + (ab._1 ^ ab._2)} == 0) true
      case _ => new DeserializationException(s"Invalid token, signature does not match")
    })

    loads(base64decode(chunks(1)))
  }

  def encode(payload: Map[String, Any], alg: String) = {
    val header: Map[String, String] = Map("typ" -> "JWT", "alg" -> alg)

    val headerFinal: String = base64encode(dumps(header))
    val payloadFinal: String = base64encode(dumps(payload))

    val mac: Mac = getAlg(Option(alg), secretKey.getBytes(charset)) match {
      case None => throw new DeserializationException(s"Unsupported algorithm")
      case Some(x) => x
    }

    val signature: String = base64encode(
      mac.doFinal((headerFinal + ('.' +: payloadFinal)).getBytes(charset))
    )

    s"$headerFinal.$payloadFinal.$signature"
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
  val sampleJSON: String = dumps(Map("x" -> "y"))

  println("[i] encoding performance")
  var a = 0
  for (a <- 1 to 10) {
    time({
      encode(Map("foo" -> "bar", "XXX" -> "XXX"), "HS512")
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
      dumps(Map("x" -> "y"))
    }, 5000)
  }

  println("[i] loads performance")
  var d = 0
  for (d <- 1 to 10) {
    time({
      loads(sampleJSON)
    }, 5000)
  }


//  println(decode(encode(Map("foo" -> "bar", "XXX" -> "XXX"), "HS512")))
  //println()

}
