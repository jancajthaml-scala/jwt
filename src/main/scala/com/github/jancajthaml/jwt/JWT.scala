package com.github.jancajthaml.jwt

import spray.json._ //spray.json.JsValue
import DefaultJsonProtocol._

import java.util.Base64

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

  def decode(token: String, encoding: String): JsValue = {
    val chunks: Array[String] = token.split('.')
    //@otod check for length here

    //@todo extract to type/function maybe
    val header: Header = new String(Base64.getDecoder().decode(chunks(0))).parseJson.convertTo[Header]

    if (header.typ != "JWT") {
      println("not JSON Web Token... invalid")
    }

    val mac: Mac = header.alg match {
      case "HS256" => { //-> HmacSHA256
        val x: Mac = Mac.getInstance("HmacSHA256")
        x.init(new SecretKeySpec("secret".getBytes(encoding), "HmacSHA256"))
        x
      }
      case "HS384" => throw new DeserializationException("HS384 not yet implemented")
      case "HS512" => throw new DeserializationException("HS512 not yet implemented")
      case "RS256" => throw new DeserializationException("RS256 not yet implemented")
      case "RS384" => throw new DeserializationException("RS384 not yet implemented")
      case "RS512" => throw new DeserializationException("RS512 not yet implemented")
      case "ES256" => throw new DeserializationException("ES256 not yet implemented")
      case "ES384" => throw new DeserializationException("ES384 not yet implemented")
      case "ES512" => throw new DeserializationException("ES512 not yet implemented")
      case x => throw new DeserializationException(s"Unsupported algorithm ${x}")
    }

    val signature = chunks(2).getBytes(encoding)
    val calculatedSignature: Array[Byte] = Base64.getEncoder().withoutPadding().encodeToString(mac.doFinal((chunks(0) + ('.' +: chunks(1))).getBytes(encoding))).getBytes(encoding)

    (calculatedSignature.length == signature.length match {
      case true => if ((signature zip calculatedSignature).foldLeft (0) {(r, ab) => r + (ab._1 ^ ab._2)} == 0) true
      case _ => new DeserializationException(s"Invalid token, signature does not match")
    })

    new String(Base64.getDecoder().decode(chunks(1))).parseJson  
  }

  println(decode(
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeFONFh7HgQ",
    "utf-8"
  ))
  //println(header.toJson)

  //println(s"payload: $payload")
}
