package com.github.jancajthaml.jwt


object base64encode extends (Any => String) {

  import com.github.jancajthaml.base64.Base64

  def apply(value: Any): String = value match {
    case value: String => Base64.getEncoder().encode(value.getBytes("utf-8"))
    case value: Array[Byte] => Base64.getEncoder().encode(value)
    case _ => ""
  }

    //Base64.getEncoder().encode(value.getBytes("utf-8"))//.split("[\\r\\n]+").mkString()

}

object base64decode extends (Any => String) {

  //performance bottleneck
  import com.github.jancajthaml.base64.Base64

  def apply(value: Any): String = value match {
    case value: String => Base64.getDecoder().decode(value.getBytes("utf-8"))
    case value: Array[Byte] => Base64.getDecoder().decode(value)
    case _ => ""
  }

}

object getAlg extends ((Option[Any], Array[Byte]) => Option[javax.crypto.Mac]) {

  //import javax.crypto.Mac
  //import javax.crypto.spec.SecretKeySpec

  def apply(name: Option[Any], key: Array[Byte]): Option[javax.crypto.Mac] = name match {
    case Some("HS256") => { //-> HmacSHA256
      val x = javax.crypto.Mac.getInstance("HmacSHA256")
      x.init(new javax.crypto.spec.SecretKeySpec(key, "HmacSHA256"))
      Some(x)
    }
    case Some("HS384") => { //-> HmacSHA384
      val x = javax.crypto.Mac.getInstance("HmacSHA384")
      x.init(new javax.crypto.spec.SecretKeySpec(key, "HmacSHA384"))
      Some(x)
    }
    case Some("HS512") => { //-> HmacSHA512
      val x = javax.crypto.Mac.getInstance("HmacSHA512")
      x.init(new javax.crypto.spec.SecretKeySpec(key, "HmacSHA512"))
      Some(x)
    }
    case Some("RS256") => { //-> SHA256withRSA
      //not available by default
      val x = javax.crypto.Mac.getInstance("SHA256withRSA")
      x.init(new javax.crypto.spec.SecretKeySpec(key, "SHA256withRSA"))
      Some(x)
    }
    case Some("RS384") => { //-> SHA384withRSA
      //not available by default
      val x = javax.crypto.Mac.getInstance("SHA384withRSA")
      x.init(new javax.crypto.spec.SecretKeySpec(key, "SHA384withRSA"))
      Some(x)
    }
    case Some("RS512") => { //-> SHA512withRSA
      //not available by default
      val x = javax.crypto.Mac.getInstance("SHA512withRSA")
      x.init(new javax.crypto.spec.SecretKeySpec(key, "SHA512withRSA"))
      Some(x)
    }
    case Some("ES256") => { //-> SHA256withECDSA
      //not available by default
      val x = javax.crypto.Mac.getInstance("SHA256withECDSA")
      x.init(new javax.crypto.spec.SecretKeySpec(key, "SHA256withECDSA"))
      Some(x)
    }
    case Some("ES384") => { //-> SHA384withECDSA
      //not available by default
      val x = javax.crypto.Mac.getInstance("SHA384withECDSA")
      x.init(new javax.crypto.spec.SecretKeySpec(key, "SHA384withECDSA"))
      Some(x)
    }
    case Some("ES512") => { //-> SHA512withECDSA
      //not available by default
      val x = javax.crypto.Mac.getInstance("SHA512withECDSA")
      x.init(new javax.crypto.spec.SecretKeySpec(key, "SHA512withECDSA"))
      Some(x)
    }
    case x => None //throw new DeserializationException(s"Unsupported algorithm ${x}")
  }

}