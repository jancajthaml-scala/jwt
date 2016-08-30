package com.github.jancajthaml.jwt

import java.util.Base64
import spray.json._ //spray.json.JsValue
import DefaultJsonProtocol._

//@todo remove these dependencies and implement from scratch
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

class Header(val alg: String, val typ: String)

object UniversalJsonProtocol extends DefaultJsonProtocol {

  implicit object HeaderJsonFormat extends RootJsonFormat[Header] {
    def write(x: Header) = {
      JsObject((Map[String, Any]() /: x.getClass.getDeclaredFields) {(a, f) =>
        f.setAccessible(true)
        a + (f.getName -> f.get(x))
      } map {case (key, value) => (key.toString -> JsString(value.toString))})
    }

    def read(value: JsValue) = { value.asJsObject.getFields("alg", "typ") match {
      case Seq(JsString(alg), JsString(typ)) => new Header(alg, typ)
      case _ => throw new DeserializationException("Header expected")
    } }
  }
}

import UniversalJsonProtocol._

object Main extends App {

  val encoding: String = "utf-8"
  val source: String = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeFONFh7HgQ"

  val chunks: Array[String] = source.split('.')
  //@otod check for length here

  //@todo extract to type/function mayne
  val header: Header = new String(Base64.getDecoder().decode(chunks(0))).parseJson.convertTo[Header]

  if (header.typ != "JWT") {
    println("not JSON Web Token... invalid")
  }

  val mac: Mac = header.alg match {
    case "HS256" => {
      //HS256 -> HmacSHA256
      println("[i] signed with HMAC SHA 256 bit")
      val x: Mac = Mac.getInstance("HmacSHA256")
      x.init(new SecretKeySpec("secret".getBytes(encoding), "HmacSHA256"))
      x
    }
    case x => throw new DeserializationException(s"Unsupported algorithm ${x}")
  }

  val signature = chunks(2).getBytes(encoding)
  val calculatedSignature: Array[Byte] = Base64.getEncoder().withoutPadding().encodeToString(mac.doFinal((chunks(0) + ('.' +: chunks(1))).getBytes(encoding))).getBytes(encoding)

  val correct: Boolean = {
    calculatedSignature.length == signature.length match {
      case true => (signature zip calculatedSignature).foldLeft (0) {(r, ab) => r + (ab._1 ^ ab._2)} == 0
      case _ => false
    }
  }

  //println(getCCParams(header))

  //println(s"header: ${header.typ} ${header.alg}")

  //@todo add check
  println(s"signature ok: $correct")

  //care about payload now
  val payload = new String(Base64.getDecoder().decode(chunks(1))).parseJson

  println(s"payload: $payload")

  println(header.toJson)

}
