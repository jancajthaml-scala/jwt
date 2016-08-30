package com.github.jancajthaml.json

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.CharArrayReader.EofCh

private[jancajthaml] class Lexer extends StdLexical with ImplicitConversions {

  override def token: Parser[Token] =
    ( string ^^ StringLit
      | EofCh ^^^ EOF
      | delim
      | '\"' ~> failure("Unterminated string")
      | rep(letter) ^^ checkKeyword
      | failure("Illegal character")
    )
  
  def checkKeyword(xs : List[Any]) = {
    val strRep = xs mkString ""
    if (reserved contains strRep) Keyword(strRep) else ErrorToken(s"Not a keyword: $strRep")
  }
  
  def string = '\"' ~> rep(charSeq | chrExcept('\"', '\n', EofCh)) <~ '\"' ^^ { _ mkString "" }
  
  override def whitespace = rep(whitespaceChar)

  private def optString[A](pre: String, a: Option[A]) = a match {
    case Some(x) => pre + x.toString
    case None => ""
  }

  def charSeq: Parser[String] =
    ('\\' ~ '\"' ^^^ "\""
    |'\\' ~ '\\' ^^^ "\\"
    |'\\' ~ '/'  ^^^ "/"
    |'\\' ~ 'b'  ^^^ "\b"
    |'\\' ~ 'f'  ^^^ "\f"
    |'\\' ~ 'n'  ^^^ "\n"
    |'\\' ~ 'r'  ^^^ "\r"
    |'\\' ~ 't'  ^^^ "\t")

}