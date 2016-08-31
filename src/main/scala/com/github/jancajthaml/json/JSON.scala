package com.github.jancajthaml.json

private[jancajthaml] object JSON extends Parser {

  def parse(input: String): Map[String, Any] = phrase(root)(new lexical.Scanner(input)) match {
    case Success(result, _) => resolveType(result).asInstanceOf[Map[String, Any]]
    case _ => Map.empty[String, Any]
  }

  def resolveType(input: List[_]): Any = {
    var x = Map[String, Any]()
  
    if (input.forall { 
      case (key: String, value: Any) =>
        x += (key -> value)
        true
      case _ => false
    }) x
    else
      input
  }

}