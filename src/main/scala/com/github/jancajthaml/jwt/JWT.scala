package com.github.jancajthaml.jwt

import java.util.Base64

//@todo remove these dependencies and implement from scratch
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object Main extends App {

  val encoding: String = "utf-8"
  val source: String = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeFONFh7HgQ"

  val chunks: Array[String] = source.split('.')
  //@otod check for length here

  //@todo extract to type/function mayne
  val header = new String(Base64.getDecoder().decode(chunks(0)))

  //@todo implement vanilla encoding for speed
  //HS256 -> HmacSHA256
  val mac: Mac = Mac.getInstance("HmacSHA256")
  mac.init(new SecretKeySpec("secret".getBytes(encoding), "HmacSHA256"))

  val signature = chunks(2).getBytes(encoding)
  val calculatedSignature: Array[Byte] = Base64.getEncoder().withoutPadding().encodeToString(mac.doFinal((chunks(0) + ('.' +: chunks(1))).getBytes(encoding))).getBytes(encoding)

  val correct: Boolean = {
    calculatedSignature.length == signature.length match {
      case true => (signature zip calculatedSignature).foldLeft (0) {(r, ab) => r + (ab._1 ^ ab._2)} == 0
      case _ => false
    }
  }

  println(s"header: $header")

  //@todo add check
  println(s"signature ok: $correct")

  //care about payload now
  val payload = new String(Base64.getDecoder().decode(chunks(1)))

  println(s"payload: $payload")

}
