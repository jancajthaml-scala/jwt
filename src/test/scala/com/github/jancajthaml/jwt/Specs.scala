package com.github.jancajthaml.jwt

import com.github.jancajthaml.json.{jsondumps, jsonloads}
import org.scalatest.{FlatSpec, Matchers}
import java.util.Base64
import scala.util.{Try,Success,Failure}

class JWTSpecs extends FlatSpec with Matchers {

  val secret: String = "secret"

  def decodeB64Native(x: String): String = new String(java.util.Base64.getDecoder().decode(x))

  "encode" should "have valid header" in {
    encode(Map(), "HS256", secret) match {
      case Success(token) => {
        val parts = token.split("\\.")

        parts should have size (3)

        val header = jsonloads(decodeB64Native(parts(0)))
        header.keys should have size (2)
        header.getOrElse("alg", None) should !== (None)
        header.getOrElse("typ", None) should === ("JWT")

      }
      case Failure(f) => {
        false should === (true)
        //@todo reject test now
      }
    }

  }

  it should "have valid payload" in {
    val payload = Map(
      "x" -> "y"
    )

    encode(payload, "HS256", secret) match {
      case Success(token) => {
        val parts = token.split("\\.")

        parts should have size (3)

        val payloadDecoded = jsonloads(decodeB64Native(parts(1)))

        payloadDecoded.keys should have size (payload.keys.size + 1)
        payloadDecoded.getOrElse("x", None) should === ("y")
      }
      case Failure(f) => {
        false should === (true)
        //@todo reject test now
      }
    }
  }

  "signature" should "support HS256" in {
    val algorithm: String = "HS256"

    encode(Map(), algorithm, secret) match {
      case Success(token) => {
        val parts = token.split("\\.")

        parts should have size (3)

        val header = jsonloads(decodeB64Native(parts(0)))

        header.getOrElse("alg", None) should === (algorithm)
      }
      case Failure(f) => {
        false should === (true)
        //@todo reject test now
      }
    }
  }

  it should "support HS384" in {
    val algorithm: String = "HS384"

    encode(Map(), algorithm, secret) match {
      case Success(token) => {
        val parts = token.split("\\.")

        parts should have size (3)

        val header = jsonloads(decodeB64Native(parts(0)))

        header.getOrElse("alg", None) should === (algorithm)
      }
      case Failure(f) => {
        false should === (true)
        //@todo reject test now
      }
    }    
  }

  it should "support HS512" in {
    val algorithm: String = "HS512"

    encode(Map(), algorithm, secret) match {
      case Success(x) => {
        val token = x
        val parts = x.split("\\.")

        parts should have size (3)

        val header = jsonloads(decodeB64Native(parts(0)))

        header.getOrElse("alg", None) should === (algorithm)
      }
      case Failure(f) => {
        false should === (true)
        //@todo reject test now
      }
    }
  }

  //RS256
  //RS384
  //RS512

  //ES256
  //ES384
  //ES512

}

class ClaimsSpecs extends FlatSpec with Matchers {

  val secret: String = "secret"

  "decode" should "validate expiration" in {
    val token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjEzMDA4MTkzODB9.DoA5WoO-SAnm7jlz7316bLAHD8Qt1CUMBVFmXTZrwcQ"

    decode(token, secret) match {
      case Success(x) => {
        false should === (true)
      }
      case Failure(f) => {
        true should === (true)
      }
    }
  }

  it should "validate issued at" in {
    val token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpYXQiOjMzMDMxMTE1NjY3fQ.vNsYl7WHXd5kPW7e7dWlsbeKDEGHOtnfI8mnIfEAvfE"

    decode(token, secret) match {
      case Success(x) => {
        false should === (true)
      }
      case Failure(f) => {
        true should === (true)
      }
    }
  }

  it should "validate use not before" in {
    val token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJuYmYiOjMzMDMxMTE1NjY3fQ.e1zTfpTdvKa6TE74NGOKhDhxwgkHe2f9X05W4xDJrxE"
    
    decode(token, secret) match {
      case Success(x) => {
        false should === (true)
      }
      case Failure(f) => {
        true should === (true)
      }
    }
  }

}
