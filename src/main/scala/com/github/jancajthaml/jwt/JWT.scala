package com.github.jancajthaml.jwt

import spray.json._ //spray.json.JsValue
import DefaultJsonProtocol._

import java.util.Base64

//@todo remove these dependencies and implement from scratch
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object UniversalJsonProtocol extends DefaultJsonProtocol {
  implicit object HeaderJsonFormat extends RootJsonFormat[Map[String, String]] {
    def write(x: Map[String, String]) = {
      JsObject(x map {case (key, value) => (key.toString -> JsString(value.toString))})
    }

    def read(value: JsValue) = { value.asJsObject.getFields("alg", "typ") match {
      case Seq(JsString(alg), JsString(typ)) => Map("alg" -> alg, "typ" -> typ)
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
    val header = new String(Base64.getDecoder().decode(chunks(0))).parseJson.convertTo[Map[String, String]]

    header.get("typ") match {
      case Some("JWT") => true
      case x => throw new DeserializationException(s"Invalid type for JWT decoding ${x}")
    }

    val mac: Mac = header.get("alg") match {
      case Some("HS256") => { //-> HmacSHA256
        val x: Mac = Mac.getInstance("HmacSHA256")
        x.init(new SecretKeySpec("secret".getBytes(encoding), "HmacSHA256"))
        x
      }
      case Some("HS384") => { //-> HmacSHA384
        val x: Mac = Mac.getInstance("HmacSHA384")
        x.init(new SecretKeySpec("secret".getBytes(encoding), "HmacSHA384"))
        x
      }
      case Some("HS512") => { //-> HmacSHA512
        val x: Mac = Mac.getInstance("HmacSHA512")
        x.init(new SecretKeySpec("secret".getBytes(encoding), "HmacSHA512"))
        x
      }
      case Some("RS256") => { //-> SHA256withRSA
        val x: Mac = Mac.getInstance("SHA256withRSA")
        x.init(new SecretKeySpec("secret".getBytes(encoding), "SHA256withRSA"))
        x
      }
      case Some("RS384") => { //-> SHA384withRSA
        val x: Mac = Mac.getInstance("SHA384withRSA")
        x.init(new SecretKeySpec("secret".getBytes(encoding), "SHA384withRSA"))
        x
      }
      case Some("RS512") => { //-> SHA512withRSA
        val x: Mac = Mac.getInstance("SHA512withRSA")
        x.init(new SecretKeySpec("secret".getBytes(encoding), "SHA512withRSA"))
        x
      }
      case Some("ES256") => { //-> SHA256withECDSA
        val x: Mac = Mac.getInstance("SHA256withECDSA")
        x.init(new SecretKeySpec("secret".getBytes(encoding), "SHA256withECDSA"))
        x
      }
      case Some("ES384") => { //-> SHA384withECDSA
        val x: Mac = Mac.getInstance("SHA384withECDSA")
        x.init(new SecretKeySpec("secret".getBytes(encoding), "SHA384withECDSA"))
        x
      }
      case Some("ES512") => { //-> SHA512withECDSA
        val x: Mac = Mac.getInstance("SHA512withECDSA")
        x.init(new SecretKeySpec("secret".getBytes(encoding), "SHA512withECDSA"))
        x
      }
      case x => throw new DeserializationException(s"Unsupported algorithm ${x}")
    }

    val signature = chunks(2).getBytes(encoding)
    val calculatedSignature: Array[Byte] = Base64.getEncoder().withoutPadding().encodeToString(
      mac.doFinal((chunks(0) + ('.' +: chunks(1))).getBytes(encoding))
    ).getBytes(encoding)

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

}
