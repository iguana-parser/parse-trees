
package iguana.parsetrees.term

import iguana.parsetrees.visitor._

import scala.collection.mutable.StringBuilder


object TermToJavaCode {

  def get(node: Term): String = {
    val treeToJavaCode = new ToJavaCode with Memoization[Term]
    treeToJavaCode.visit(node)
    treeToJavaCode.get
  }

}

private class ToJavaCode extends Visitor[Term] with Id {

  type T = Unit

  def get: String = sb.toString

  val sb = new StringBuilder

  override def visit(node: Term): VisitResult[Unit] = node match {

    case TerminalTerm(slot, i, j, input) =>
      sb ++= s"""Tree t${getId(node)} = createTerminal($slot, $i, $j, input);\n"""
      None

    case NonterminalTerm(r, children, input) =>
      children.foreach(visit(_))
      val label = "list(" + children.map(c => "t" + getId(c)).mkString(", ") + ")"
      sb ++= s"""Tree t${getId(node)} = createRule($r, $label, input);\n"""
      None

     case AmbiguityTerm(branches: Seq[Seq[Term]]) =>
       branches.foreach(b => b.foreach(c => visit(c)))
       val label = branches.map(b => "list(" + b.map(c => "t" + getId(c)).mkString(", ") + ")").mkString(", ")
       sb ++= s"""Tree t${getId(node)} = createAmbiguity(list($label));\n"""
       None

    case Epsilon(i) =>
      sb ++= s"""Tree t${getId(node)} = createEpsilon($i);\n"""
      None

    case Cycle(label) =>
      sb ++= s"""Tree t${getId(node)} = createCycle($label);\n"""
      None

    case Star(children) =>
      children.foreach(visit(_))
      val label = "list(" + children.map(c => "t" + getId(c)).mkString(", ") + ")"
      sb ++= s"""Tree t${getId(node)} = createStar($label);\n"""
      None

    case Plus(children) =>
      children.foreach(visit(_))
      val label = "list(" + children.map(c => "t" + getId(c)).mkString(", ") + ")"
      sb ++= s"""Tree t${getId(node)} = createPlus($label);\n"""
      None

    case Group(children) =>
      children.foreach(visit(_))
      val label = "list(" + children.map(c => "t" + getId(c)).mkString(", ") + ")"
      sb ++= s"""Tree t${getId(node)} = createGroup($label);\n"""
      None

    case Opt(child) =>
      visit(child)
      sb ++= s"""Tree t${getId(node)} = createOpt(t${getId(child)});\n"""
      None

    case Alt(children) =>
      children.foreach(visit(_))
      val label = "list(" + children.map(c => "t" + getId(c)).mkString(", ") + ")"
      sb ++= s"""Tree t${getId(node)} = createAlt($label);\n"""
      None
  }
}
