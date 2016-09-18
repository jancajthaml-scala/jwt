package com.github.jancajthaml.jwt

import scala.util.{Try,Success,Failure}

object encode extends ((Map[String, Any], String, String) => scala.util.Try[String]) {

  import com.github.jancajthaml.json.{jsondumps}

  def apply(body: Map[String, Any], alg: String, secret: String): scala.util.Try[String] = {
    //for rainbow table prevention, shuffle values in header from time to time
    val header: String = base64encode(jsondumps(Map("alg" -> alg, "typ" -> "JWT")))
    val extendedBody: Map[String, Any] = Map(
      "iat" -> (System.currentTimeMillis() / 1000).asInstanceOf[Int]
    ) ++ body
    //val now = 
    val payload: String = base64encode(jsondumps(extendedBody))
    val signature = getAlg(Option(alg), secret.getBytes("utf-8")) match {
      case None => throw new Exception(s"Unsupported algorithm")
      case Some(x) => base64encode(x.doFinal((header + ('.' +: payload)).getBytes("utf-8")))
    }

    Success(header + ('.' +: payload) + ('.' +: (signature)))
  }
}

object decode extends ((String, String) => scala.util.Try[Map[String, Any]]) {
  
  import com.github.jancajthaml.json.{jsonloads}

  def apply(token: String, secret: String): scala.util.Try[Map[String, Any]] = {
    val now: Int = (System.currentTimeMillis() / 1000).asInstanceOf[Int]

    val chunks: Array[String] = token.split('.')
    val header = jsonloads(base64decode(chunks(0)))
    header.get("typ") match {
      case Some("JWT") => {}
      case Some("JWE") => throw new Exception("Nested encryption not supported, \"typ: JWE\"")
      case x => throw new Exception(s"Invalid type for JWT decoding ${x}")
    }
    val signature = chunks(2).getBytes("utf-8")
    val calculatedSignature: Array[Byte] = (getAlg(header.get("alg"), secret.getBytes("utf-8")) match {
      case None => throw new Exception(s"Unsupported algorithm")
      case Some(x) => base64encode(
        x.doFinal((chunks(0) + ('.' +: chunks(1))).getBytes("utf-8"))
      ).getBytes("utf-8")
    })

    calculatedSignature.length == signature.length match {
      case true => if ((signature zip calculatedSignature).foldLeft (0) {(r, ab) => r + (ab._1 ^ ab._2)} == 0) true
      case _ => throw new Exception("Invalid token, signature does not match")
    }

    val body = jsonloads(base64decode(chunks(1)))

    body.get("iat") match {
      case None => {}
      case x => if (x.get.asInstanceOf[Int] > now) {
        throw new Exception("Token is issued at future (iat validation failed)")
      }
    }

    body.get("exp") match {
      case None => {}
      case x => if (x.get.asInstanceOf[Int] < now) {
        throw new Exception("Token is expired (exp validation failed)")
      }
    }

    body.get("nbf") match {
      case None => {}
      case x => if (x.get.asInstanceOf[Int] > now) {
        throw new Exception("Used too early (nbf validation failed)")
      }
    }

    Try(body)
  }
}
