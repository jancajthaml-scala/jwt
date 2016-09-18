package com.github.jancajthaml.jwt

object encode extends ((Map[String, Any], String, String) => scala.util.Try[String]) {

  def apply(body: Map[String, Any], alg: String, secret: String): scala.util.Try[String] = {
    scala.util.Try({
      val header: String = serialize(Map("alg" -> alg, "typ" -> "JWT"))
      val payload: String = serialize(Map("iat" -> now()) ++ body)
      val signature = sign(header, payload, alg, secret)
      header + ('.' +: payload) + ('.' +: (signature))
    })
  }
}

object decode extends ((String, String) => scala.util.Try[Map[String, Any]]) {

  def apply(token: String, secret: String): scala.util.Try[Map[String, Any]] = {
    scala.util.Try({
      val time: Int = now()
      val chunks: Array[String] = token.split('.')
      val header = deserialize(chunks(0))
      header.get("typ") match {
        case Some("JWT") => {}
        case Some("JWE") => throw new Exception("Nested encryption not supported, \"typ: JWE\"")
        case x => throw new Exception(s"Invalid type for JWT decoding ${x}")
      }
      val signature = chunks(2).getBytes("utf-8")
      val calculatedSignature = sign(chunks(0), chunks(1), header.get("alg"), secret)

      calculatedSignature.length == signature.length match {
        case true => if ((signature zip calculatedSignature).foldLeft (0) {
          (r, ab) => r + (ab._1 ^ ab._2)} == 0) true
        case _ => throw new Exception("Invalid token, signature does not match")
      }

      val body = deserialize(chunks(1))

      body.get("iat") match {
        case None => {}
        case x => if (x.get.asInstanceOf[Int] > time) {
          throw new Exception("Token is issued at future (iat validation failed)")
        }
      }

      body.get("exp") match {
        case None => {}
        case x => if (x.get.asInstanceOf[Int] < time) {
          throw new Exception("Token is expired (exp validation failed)")
        }
      }

      body.get("nbf") match {
        case None => {}
        case x => if (x.get.asInstanceOf[Int] > time) {
          throw new Exception("Used too early (nbf validation failed)")
        }
      }

      body
    })
  }
}
