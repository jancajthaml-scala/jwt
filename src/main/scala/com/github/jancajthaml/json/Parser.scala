package com.github.jancajthaml.json

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

private[jancajthaml] class Parser extends StdTokenParsers with ImplicitConversions {

  type Tokens = Lexer
  val lexical = new Tokens

  lexical.reserved ++= List("true", "false", "null")
  lexical.delimiters ++= List("{", "}", ":", ",")

  def root = "{" ~> repsep(objEntry, ",") <~ "}"
  def objEntry = string ~ (":" ~> value) ^^ { case x ~ y => (x, y) }
  def value: Parser[Any] = (root | "true" ^^^ true | "false" ^^^ false | "null" ^^^ null | string)
  def string = accept("string", { case lexical.StringLit(n) => n} )
}