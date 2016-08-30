package com.github.jancajthaml.json

object JSON extends Parser {

  def parse(input: String): Option[Any] = phrase(root)(new lexical.Scanner(input)) match {
    case Success(result, _) => Some(resolveType(result))
    case _ => None
  }

  def resolveType(input: List[_]): Any = {
    var x = Map[String, Any]()
  
    if (input.forall { 
      case (key: String, value: List[_]) =>
        x += (key -> resolveType(value))
        true
      case (key : String, value : Any) =>
        x += (key -> value)
        true
      case _ => false
    }) x
    else
      input
  }

}