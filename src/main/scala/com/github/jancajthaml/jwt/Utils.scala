package com.github.jancajthaml.jwt

object jsondumps extends (Map[String, Any] => String) {
  
  def apply(value: Map[String, Any]): String = {
    //@todo string construction hogs this, should refactor to
    //recursion returning chunks of Array[Byte] in sudo parallel
    val q = '"'
    "{" + value.map(x => {
      //perf problem in string concat maybe?
      x._2 match {
        case d: String => s"$q${x._1}$q:$q${x._2}$q"
        case null => s"$q${x._1}$q:null"
        case c => s"$q${x._1}$q:$c"
      }
    //better string buffering
    }).mkString("",",","") + "}" //perf problem at this line
  }
}

object jsonloads extends (String => Map[String, Any]) {

  def apply(value: String): Map[String, Any] = {
    //@todo check string contains nested json (more than one "{" or "}")
    //because we do not support nested json strucures @todo TBD
    var loaded = Map[String, Any]()
    /*
      (t|f) => boolean (true|false)
      (0|1|2|3|4|5|6|7|8|9) => integral number (decimals unsuported)
      (") => string
      (n) => null
      (u) => undefined or unicode ... skip
      (e) => decimal number sci notation ... skip
    */
    val char2fn = Map(
      '"' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> v.drop(1).dropRight(1))),
      't' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> true)),
      'f' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> false)),
      'n' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> null)),
      '0' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> v.toInt)),
      '1' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> v.toInt)),
      '2' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> v.toInt)),
      '3' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> v.toInt)),
      '4' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> v.toInt)),
      '5' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> v.toInt)),
      '6' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> v.toInt)),
      '7' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> v.toInt)),
      '8' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> v.toInt)),
      '9' -> ((k:String, v:String, r:Map[String, Any]) => r + (k -> v.toInt))
    )

    val pipe = ((k:String, v:String, r:Map[String, Any]) => r)

    //@todo slows down parsing 4 times, need better regex in better times
    value.replaceAll("""[\r\n{}]+""", "").trim().split(",").filter(_.nonEmpty).map(x => {
      val t = x.split("\":")
      //@todo these two regexes are bad, should be done on value beforehand
      val v = t(1).replaceAll("""^[ \t]+|[ \t]+$""", "")
      val k = t(0).replaceAll("""^[\"\' \t]+|[\"\' \t]+$""", "")
      loaded = char2fn.getOrElse(v(0), pipe)(k, v, loaded)
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