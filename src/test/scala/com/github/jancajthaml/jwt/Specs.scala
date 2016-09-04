package com.github.jancajthaml.jwt

import com.github.jancajthaml.json.{jsondumps, jsonloads}

import collection.mutable.Stack
import org.scalatest._

import java.util.Base64
import scala.util.{Try,Success,Failure}

class JSONSpecs extends FlatSpec with Matchers {

  val secret: String = "secret"

  def decodeB64Native(x: String): String = new String(java.util.Base64.getDecoder().decode(x))

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

  it should "support HS256 signature" in {
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

}