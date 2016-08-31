package com.github.jancajthaml.jwt

object jsondumps extends (Map[String, Any] => String) {
  
  def apply(value: Map[String, Any]): String = {
    //if x._2 is instance of Map go to recursion
    //if x._2 is instance of Number, true, false, Nil|None dont wrap in quotes
    //if x._2 is instance of List then ignore (wont support)
    "{" + value.map(x => {("\"" + x._1 + "\":\"" + x._2 + "\"")}).mkString("",", ","") + "}"
  }
}

object jsonloads extends (String => Map[String, Any]) {

  def apply(value: String): Map[String, Any] = {
    var loaded = Map[String, Any]()
    value.replaceAll("""[\r\n{}]+""", "").trim().split(",").foreach(x => x.split("\":") match {
      case Array(x: String, y: String) => {
        loaded += (
          x.replaceAll("""^[\"\' \t]+||^[ \t]+$""", "") ->
          //if replaced y == "y" then it was a string, otherwise post-process
          y.replaceAll("""^[\"\' \t]+|[\"\' \t]+$""", "") //add check to define more rigerous type (false, true, null, number...)
        )
      }
    })
    loaded
  }
}

object base64encode extends (Any => String) {

  import com.github.jancajthaml.base64.Base64

  def apply(value: Any): String = value match {
    case value: String => Base64.getEncoder().encode(value.replaceAll("[\\r\\n]+", "").getBytes("utf-8"))
    case value: Array[Byte] => Base64.getEncoder().encode(value)
    case _ => ""
  }
}

object base64decode extends (Any => String) {

  import com.github.jancajthaml.base64.Base64

  def apply(value: Any): String = value match {
    case value: String => Base64.getDecoder().decode(value.getBytes("utf-8")).replaceAll("[\\r\\n]+", "")
    case value: Array[Byte] => Base64.getDecoder().decode(value).replaceAll("[\\r\\n]+", "")
    case _ => ""
  }
}

object getAlg extends ((Option[Any], Array[Byte]) => Option[javax.crypto.Mac]) {

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
    case x => None
  }
}