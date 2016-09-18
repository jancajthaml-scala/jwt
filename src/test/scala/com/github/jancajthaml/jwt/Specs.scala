package com.github.jancajthaml.jwt

import com.github.jancajthaml.json.{jsondumps, jsonloads}

import collection.mutable.Stack
import org.scalatest._

import java.util.Base64
import scala.util.{Try,Success,Failure}

class JSONSpecs extends FlatSpec with Matchers {

  val secret: String = "secret"

  def decodeB64Native(x: String): String = new String(java.util.Base64.getDecoder().decode(x))
/*
  "decode" should "support valid JWT" in {

  }
*/
  "encode" should "have valid header" in {
    var parts = Array.empty[String]
    var token:String = ""

    encode(Map(), "HS256", secret) match {
      case Success(x) => {
        token = x
        parts = x.split("\\.")
      }
      case Failure(f) => {
      }
    }

    parts should have size (3)

    val header = jsonloads(decodeB64Native(parts(0)))

    header.keys should have size (2)
    header.getOrElse("alg", None) should !== (None)
    header.getOrElse("typ", None) should === ("JWT") 
  }

  it should "have valid payload" in {
    var parts = Array.empty[String]
    val payload = Map(
      "x" -> "y"
    )
    var token:String = ""

    encode(payload, "HS256", secret) match {
      case Success(x) => {
        token = x
        parts = x.split("\\.")
      }
      case Failure(f) => {
        //@todo reject test now
      }
    }

    parts should have size (3)

    val payloadDecoded = jsonloads(decodeB64Native(parts(1)))

    payloadDecoded.keys should have size (payload.keys.size + 1)
    payloadDecoded.getOrElse("x", None) should === ("y")
  }

  "signature" should "support HS256" in {
    val algorithm: String = "HS256"
    var parts = Array.empty[String]
    var token:String = ""

    encode(Map(), algorithm, secret) match {
      case Success(x) => {
        token = x
        parts = x.split("\\.")
      }
      case Failure(f) => {
      }
    }

    parts should have size (3)

    val header = jsonloads(decodeB64Native(parts(0)))

    header.getOrElse("alg", None) should === (algorithm)
    
  }

  it should "support HS384" in {
    val algorithm: String = "HS384"
    var parts = Array.empty[String]
    var token:String = ""

    encode(Map(), algorithm, secret) match {
      case Success(x) => {
        token = x
        parts = x.split("\\.")
      }
      case Failure(f) => {
      }
    }

    parts should have size (3)

    val header = jsonloads(decodeB64Native(parts(0)))

    header.getOrElse("alg", None) should === (algorithm)
    
  }

  it should "support HS512" in {
    val algorithm: String = "HS512"
    var parts = Array.empty[String]
    var token:String = ""

    encode(Map(), algorithm, secret) match {
      case Success(x) => {
        token = x
        parts = x.split("\\.")
      }
      case Failure(f) => {
      }
    }

    parts should have size (3)

    val header = jsonloads(decodeB64Native(parts(0)))

    header.getOrElse("alg", None) should === (algorithm)
    
  }

  //RS256
  //RS384
  //RS512

  //ES256
  //ES384
  //ES512

  "decode" should "validate expiration" in {
    val token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjEzMDA4MTkzODB9.DoA5WoO-SAnm7jlz7316bLAHD8Qt1CUMBVFmXTZrwcQ"
    an [Exception] should be thrownBy decode(token, secret)
  }

  it should "validate issued at" in {
    val token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpYXQiOjMzMDMxMTE1NjY3fQ.vNsYl7WHXd5kPW7e7dWlsbeKDEGHOtnfI8mnIfEAvfE"
    an [Exception] should be thrownBy decode(token, secret)
  }

  it should "validate use not before" in {
    val token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJuYmYiOjMzMDMxMTE1NjY3fQ.e1zTfpTdvKa6TE74NGOKhDhxwgkHe2f9X05W4xDJrxE"
    an [Exception] should be thrownBy decode(token, secret)
  }
  

}