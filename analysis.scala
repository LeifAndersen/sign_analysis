object Expresion {

  val StatementMap = scala.collection.mutable.HashMap.empty[String,List[Any]];

  def preprocess(code: List[Any]): Any = code match {
    case List(List("label", label: String), rest: List[Any]) => {
      StatementMap += (label -> rest)
      preprocess(rest)
    }
    case List(_, rest: List[Any]) => preprocess(rest)
    case _ => null
  }

  def alpha(n: Integer): Set[String] = {
    if(n < 0) {
      Set("-")
    } else if(n > 0) {
      Set("+")
    } else {
      Set("0")
    }
  }

  def plusAbstract(s1: List[String], s2: List[String]): Set[String] = {
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

  def multiplyAbstract(s1: List[String], s2: List[String]): Set[String] = {
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

  def main(args: Array[String]) {
    var code = List()
    preprocess(List(List("label", "lab"), List("3")))
    println(StatementMap)
  }
}

