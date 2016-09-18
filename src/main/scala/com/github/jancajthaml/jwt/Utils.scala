package com.github.jancajthaml.jwt

import com.github.jancajthaml.base64.Base64
import com.github.jancajthaml.json.{jsondumps, jsonloads}

object sign extends ((String, String, Any, String) => String) {
  def apply(header: String, payload: String, alg: Any, secret: String): String = alg match {
    case "HS256" | Some("HS256") => { //-> HmacSHA256
      val x = javax.crypto.Mac.getInstance("HmacSHA256")
      x.init(new javax.crypto.spec.SecretKeySpec(secret.getBytes("utf-8"), "HmacSHA256"))
      Base64.getEncoder().encode(x.doFinal((header + ('.' +: payload)).getBytes("utf-8")))
    }
    case "HS384" | Some("HS384") => { //-> HmacSHA384
      val x = javax.crypto.Mac.getInstance("HmacSHA384")
      x.init(new javax.crypto.spec.SecretKeySpec(secret.getBytes("utf-8"), "HmacSHA384"))
      Base64.getEncoder().encode(x.doFinal((header + ('.' +: payload)).getBytes("utf-8")))
    }
    case "HS512" | Some("HS512") => { //-> HmacSHA512
      val x = javax.crypto.Mac.getInstance("HmacSHA512")
      x.init(new javax.crypto.spec.SecretKeySpec(secret.getBytes("utf-8"), "HmacSHA512"))
      Base64.getEncoder().encode(x.doFinal((header + ('.' +: payload)).getBytes("utf-8")))
    }
    case "RS256" | Some("RS256") => { //-> SHA256withRSA
      //not available by default
      val x = javax.crypto.Mac.getInstance("SHA256withRSA")
      x.init(new javax.crypto.spec.SecretKeySpec(secret.getBytes("utf-8"), "SHA256withRSA"))
      Base64.getEncoder().encode(x.doFinal((header + ('.' +: payload)).getBytes("utf-8")))
    }
    case "RS384" | Some("RS384") => { //-> SHA384withRSA
      //not available by default
      val x = javax.crypto.Mac.getInstance("SHA384withRSA")
      x.init(new javax.crypto.spec.SecretKeySpec(secret.getBytes("utf-8"), "SHA384withRSA"))
      Base64.getEncoder().encode(x.doFinal((header + ('.' +: payload)).getBytes("utf-8")))
    }
    case "RS512" | Some("RS512") => { //-> SHA512withRSA
      //not available by default
      val x = javax.crypto.Mac.getInstance("SHA512withRSA")
      x.init(new javax.crypto.spec.SecretKeySpec(secret.getBytes("utf-8"), "SHA512withRSA"))
      Base64.getEncoder().encode(x.doFinal((header + ('.' +: payload)).getBytes("utf-8")))
    }
    case "ES256" | Some("ES256") => { //-> SHA256withECDSA
      //not available by default
      val x = javax.crypto.Mac.getInstance("SHA256withECDSA")
      x.init(new javax.crypto.spec.SecretKeySpec(secret.getBytes("utf-8"), "SHA256withECDSA"))
      Base64.getEncoder().encode(x.doFinal((header + ('.' +: payload)).getBytes("utf-8")))
    }
    case "ES384" | Some("ES384") => { //-> SHA384withECDSA
      //not available by default
      val x = javax.crypto.Mac.getInstance("SHA384withECDSA")
      x.init(new javax.crypto.spec.SecretKeySpec(secret.getBytes("utf-8"), "SHA384withECDSA"))
      Base64.getEncoder().encode(x.doFinal((header + ('.' +: payload)).getBytes("utf-8")))
    }
    case "ES512" | Some("ES512") => { //-> SHA512withECDSA
      //not available by default
      val x = javax.crypto.Mac.getInstance("SHA512withECDSA")
      x.init(new javax.crypto.spec.SecretKeySpec(secret.getBytes("utf-8"), "SHA512withECDSA"))
      Base64.getEncoder().encode(x.doFinal((header + ('.' +: payload)).getBytes("utf-8")))
    }
    case x => throw new Exception(s"Unsupported algorithm")
  }
}

object now extends (() => Int) {
  def apply(): Int = (System.currentTimeMillis() / 1000).asInstanceOf[Int]
}

object serialize extends (Map[String,Any] => String) {
  def apply(value: Map[String,Any]): String =
    Base64.getEncoder().encode(jsondumps(value).getBytes("utf-8"))
}

object deserialize extends (String => Map[String,Any]) {
  def apply(value: String): Map[String,Any] =
    jsonloads(Base64.getDecoder().decode(value.getBytes("utf-8")).replaceAll("[\\r\\n]+", ""))
}
