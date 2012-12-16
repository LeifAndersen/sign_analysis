

abstract class SExp {
  def isList: Boolean
  def isAtom: Boolean
}

class ListSExp(token: String) extends Statement {
  override def isList = true
  override def isAtom = false
}

class ASExp(token: List[Statement]) extends Statement {
  override def isList = false
  override def isAtom = true
}

object Expresion {

  val StatementMap = scala.collection.mutable.HashMap.empty[String,List[Any]];

  def preprocess(code: List[Any]) = this match {
    case List(List("label", label: String), rest: List) => {
      StatementMap += (label -> rest)
      preprocess(rest)
    }
    case List(a, rest: List) => preprocess(rest)
    case _ => null
  }

  def main(args: Array[String]) {
    preprocess(List(List("label", "lab"), 3))
    println(StatementMap.type)
  }
}
