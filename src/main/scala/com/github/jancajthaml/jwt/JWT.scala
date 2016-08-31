package com.github.jancajthaml.jwt

import com.github.jancajthaml.json.JSON.{parse => loads}

import java.util.Base64

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object Main extends App {

  val secretKey: String = "secret"

  def dumps(value: Map[String, Any]): String = {
    "{" + value.map(x => {("\"" + x._1 + "\":\"" + x._2 + "\"")}).mkString("",", ","") + "}"
  }

  def getAlg(name: Option[Any], key: Array[Byte]): Option[Mac] = {
    /*val mac: Mac = */
    name match {
      case Some("HS256") => { //-> HmacSHA256
        val x: Mac = Mac.getInstance("HmacSHA256")
        x.init(new SecretKeySpec(key, "HmacSHA256"))
        Some(x)
      }
      case Some("HS384") => { //-> HmacSHA384
        val x: Mac = Mac.getInstance("HmacSHA384")
        x.init(new SecretKeySpec(key, "HmacSHA384"))
        Some(x)
      }
      case Some("HS512") => { //-> HmacSHA512
        val x: Mac = Mac.getInstance("HmacSHA512")
        x.init(new SecretKeySpec(key, "HmacSHA512"))
        Some(x)
      }
      case Some("RS256") => { //-> SHA256withRSA
        //not available by default
        val x: Mac = Mac.getInstance("SHA256withRSA")
        x.init(new SecretKeySpec(key, "SHA256withRSA"))
        Some(x)
      }
      case Some("RS384") => { //-> SHA384withRSA
        //not available by default
        val x: Mac = Mac.getInstance("SHA384withRSA")
        x.init(new SecretKeySpec(key, "SHA384withRSA"))
        Some(x)
      }
      case Some("RS512") => { //-> SHA512withRSA
        //not available by default
        val x: Mac = Mac.getInstance("SHA512withRSA")
        x.init(new SecretKeySpec(key, "SHA512withRSA"))
        Some(x)
      }
      case Some("ES256") => { //-> SHA256withECDSA
        //not available by default
        val x: Mac = Mac.getInstance("SHA256withECDSA")
        x.init(new SecretKeySpec(key, "SHA256withECDSA"))
        Some(x)
      }
      case Some("ES384") => { //-> SHA384withECDSA
        //not available by default
        val x: Mac = Mac.getInstance("SHA384withECDSA")
        x.init(new SecretKeySpec(key, "SHA384withECDSA"))
        Some(x)
      }
      case Some("ES512") => { //-> SHA512withECDSA
        //not available by default
        val x: Mac = Mac.getInstance("SHA512withECDSA")
        x.init(new SecretKeySpec(key, "SHA512withECDSA"))
        Some(x)
      }
      case x => None //throw new DeserializationException(s"Unsupported algorithm ${x}")
    }
  }

  def decode(token: String, encoding: String): Map[String, Any] = {
    val chunks: Array[String] = token.split('.')
    //@todo check for length here

    //@todo extract to type/function maybe
    val header = loads(new String(Base64.getDecoder().decode(chunks(0))))

    header.get("typ") match {
      case Some("JWT") => true
      case x => throw new DeserializationException(s"Invalid type for JWT decoding ${x}")
    }

    val mac: Mac = getAlg(header.get("alg"), secretKey.getBytes(encoding)) match {
      case None => throw new DeserializationException(s"Unsupported algorithm")
      case Some(x) => x
    }

    val signature = chunks(2).getBytes(encoding)
    val calculatedSignature: Array[Byte] = Base64.getEncoder().withoutPadding().encodeToString(
      mac.doFinal((chunks(0) + ('.' +: chunks(1))).getBytes(encoding))
    ).getBytes(encoding)

    //fast
    (calculatedSignature.length == signature.length match {
      case true => if ((signature zip calculatedSignature).foldLeft (0) {(r, ab) => r + (ab._1 ^ ab._2)} == 0) true
      case _ => new DeserializationException(s"Invalid token, signature does not match")
    })

    loads(new String(Base64.getDecoder().decode(chunks(1))))
  }

  def encode(payload: Map[String, Any], alg: String, encoding: String) = {
    val header: Map[String, String] = Map("typ" -> "JWT", "alg" -> alg)

    val headerFinal: String = new String(Base64.getEncoder().withoutPadding().encodeToString(dumps(header).getBytes(encoding)))
    val payloadFinal: String = new String(Base64.getEncoder().withoutPadding().encodeToString(dumps(payload).getBytes(encoding)))

    val mac: Mac = getAlg(Option(alg), secretKey.getBytes(encoding)) match {
      case None => throw new DeserializationException(s"Unsupported algorithm")
      case Some(x) => x
    }

    val signature: String = new String(Base64.getEncoder().withoutPadding().encodeToString(
      mac.doFinal((headerFinal + ('.' +: payloadFinal)).getBytes(encoding))
    ))

    s"$headerFinal.$payloadFinal.$signature"
  }

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
  for (a <- 1 to 5) {
    time({
      encode(Map("foo" -> "bar", "XXX" -> "XXX"), "HS512", "utf-8")
    }, 1000)
  }

  println("[i] decoding performance")
  var b = 0
  for (b <- 1 to 5) {
    time({
      decode(sampleJWT, "utf-8")
    }, 1000)
  }

  println("[i] dumps performance")
  var c = 0
  for (c <- 1 to 5) {
    time({
      dumps(Map("x" -> "y"))
    }, 1000)
  }

  println("[i] loads performance")
  var d = 0
  for (d <- 1 to 5) {
    time({
      loads(sampleJSON)
    }, 1000)
  }

}
