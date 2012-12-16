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

  def main(args: Array[String]) {
    var code = List()
    preprocess(List(List("label", "lab"), List("3")))
    println(StatementMap)
  }
}
