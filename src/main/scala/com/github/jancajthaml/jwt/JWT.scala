package com.github.jancajthaml.jwt

//@todo add copy function that just prolong issued at and expiration if present and
//patches iss with this method if present

//@todo have secret in UTF8 Array[Byte] maybe?

object encode extends ((Map[String, Any], String, String) => scala.util.Try[String]) {

  def apply(body: Map[String, Any], alg: String, secret: String): scala.util.Try[String] = {
    scala.util.Try({
      val header: String = serialize(Map("alg" -> alg, "typ" -> "JWT"))
      val payload: String = serialize(Map("iat" -> now()) ++ body)
      header + ('.' +: payload) + ('.' +: (sign(header, payload, alg, secret)))
    })
  }
}

object decode extends ((String, String) => scala.util.Try[Map[String, Any]]) {

  def apply(token: String, secret: String): scala.util.Try[Map[String, Any]] = {
    scala.util.Try({
      val time: Long = now()
      val chunks: Array[String] = token.split('.')
      val header: Map[String,Any] = deserialize(chunks(0))
      header.get("typ") match {
        case Some("JWT") => {}
        case Some("JWE") => throw new Exception("Nested encryption not supported, \"typ: JWE\"")
        case x => throw new Exception(s"Invalid type for JWT decoding ${x}")
      }
      val signature: Array[Byte] = chunks(2).getBytes("utf-8")
      val calculatedSignature: Array[Byte] = sign(chunks(0), chunks(1), header.get("alg"), secret).getBytes("utf-8")

      calculatedSignature.length == signature.length match {
        case true => if ((signature zip calculatedSignature).foldLeft (0) {
          (r, ab) => r + (ab._1 ^ ab._2)} == 0) true
        case _ => throw new Exception("Invalid token, signature does not match")
      }

      val body: Map[String,Any] = deserialize(chunks(1))

      body.get("iat") match {
        case None => {}
        case Some(x) => if (x.asInstanceOf[Long] > time)
          throw new Exception("Token is issued at future (iat validation failed)")
      }

      body.get("exp") match {
        case None => {}
        case Some(x) => if (x.asInstanceOf[Long] < time)
          throw new Exception("Token is expired (exp validation failed)")
      }

      body.get("nbf") match {
        case None => {}
        case Some(x) => if (x.asInstanceOf[Long] > time)
          throw new Exception("Used too early (nbf validation failed)")
      }

      body
    })
  }
}
