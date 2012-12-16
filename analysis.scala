case class AState(statement: List[Any], env: Map[String, Set[String]])

object Expresion {

  var StatementMap = Map.empty[String,List[Any]];

  def preprocess(code: List[Any]): Any = code match {
    case List(List("label", label: String), rest: List[Any]) => {
      StatementMap += (label -> rest)
      preprocess(rest)
    }
    case List(_, rest: List[Any]) => preprocess(rest)
    case List() => null
    case _ => "error"
  }

  def alpha(n: Int): Set[String] = {
    if(n < 0) {
      Set("-")
    } else if(n > 0) {
      Set("+")
    } else {
      Set("0")
    }
  }

  def plusAbstract(s1: Set[String], s2: Set[String]): Set[String] = {
    var x = Set[String]()
    for(i <- s1) {
      for(j <- s2) {
        x = x++plusAlpha(i, j)
      }
    }
    x
  }

  def plusAlpha(s1: String, s2: String): Set[String] = {
    if(s1 == "-" && s2 == "-") {
      Set("-")
    } else if(s1 == "-" && s2 == "0") {
      Set("-")
    } else if(s1 == "-" && s2 == "+") {
      Set("-", "0", "+")
    } else if(s1 == "0") {
      Set(s2)
    } else if(s1 == "+" && s2 == "-") {
      Set("-", "0", "+")
    } else if(s1 == "+" && s2 == "0") {
      Set("+")
    } else if(s1 == "+" && s2 == "+") {
      Set("+")
    } else {
      Set("Error")
    }
  }

  def multiplyAbstract(s1: Set[String], s2: Set[String]): Set[String] = {
    var x = Set[String]()
    for(i <- s1) {
      for(j <- s2) {
        x = x++multiplyAlpha(i, j)
      }
    }
    x
  }

  def multiplyAlpha(s1: String, s2: String): Set[String] = {
    if(s1 == "-" && s2 == "-") {
      Set("-")
    } else if(s1 == "-" && s2 == "0") {
      Set("0")
    } else if(s1 == "-" && s2 == "+") {
      Set("-")
    } else if(s1 == "0") {
      Set("0");
    } else if(s1 == "+" && s2 == "-") {
      Set("-")
    } else if(s1 == "+" && s2 == "0") {
      Set("0")
    } else if(s1 == "+" && s2 == "+") {
      Set("+")
    } else {
      Set("Error")
    }
  }

  def ExpAEval(exp: Any, aenv: Map[String, Set[String]]): Set[String] = exp match {
    case a: Int => alpha(a)
    case a: String => aenv(a)
    case List("+", a, b) => plusAbstract(ExpAEval(a, aenv), ExpAEval(b, aenv))
    case List("*", a, b) => multiplyAbstract(ExpAEval(a, aenv), ExpAEval(b, aenv))
    case List("=", a, b) => Set("0", "+")
    case _ => Set("error")
  }

  def astep(astate0: AState): Any = astate0 statement match {
    case List() => astate0 env
    case List(List("label", lab: String), rest: List[Any]) => List(AState(rest, astate0 env))
    case List(List(":=", va: String, exp: Any), rest: List[Any]) => {
      val aenv2 = astate0.env++Map(va -> ExpAEval(exp, astate0 env))
      List(AState(rest, aenv2))
    }
    case List(List("goto", lab: String), rest: List[Any]) => List(AState(StatementMap(lab), astate0 env))
    case List(List("if", exp: Any, "goto", lab: String), rest: List[Any]) => List(
      AState(StatementMap(lab), astate0 env),
      AState(rest, astate0 env))
    case _ => "error"
  }

  def main(args: Array[String]) {
    var code = List()
    preprocess(List(List("label", "lab"), List(List("label", "lab2"), List(List("goto", "got"), List()))))
    println(StatementMap)
  }
}

