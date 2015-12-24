
package iguana.parsetrees.term

import iguana.parsetrees.visitor._

import scala.collection.mutable.StringBuilder


object TermToScalaCode {

  def get(node: Term): String = {
    val treeToScalaCode = new ToScalaCode with Memoization[Term]
    treeToScalaCode.visit(node)
    treeToScalaCode.get
  }

}

private class ToScalaCode extends Visitor[Term] with Id {

  type T = Unit

  def get: String = sb.toString

  val sb = new StringBuilder

  override def visit(node: Term): VisitResult[Unit] = node match {

    case TerminalTerm(slot, i, j, input) =>
      sb ++= s"""val t${getId(node)} = TerminalTerm($slot, $i, $j, input)\n"""
      None

    case NonterminalTerm(r, children, input) =>
      children.foreach(visit(_))
      val label = "list(" + children.map(c => "t" + getId(c)).mkString(", ") + ")"
      sb ++= s"""val t${getId(node)} = NonterminalTerm($r, $label, input)\n"""
      None

     case AmbiguityTerm(branches: Seq[Seq[Term]]) =>
       branches.foreach(b => b.foreach(c => visit(c)))
       val label = branches.map(b => "list(" + b.map(c => "t" + getId(c)).mkString(", ") + ")").mkString(", ")
       sb ++= s"""val t${getId(node)} = AmbiguityTerm(list($label))\n"""
       None

    case Epsilon(i) =>
      sb ++= s"""val t${getId(node)} = Epsilon($i)\n"""
      None

    case Cycle(label) =>
      sb ++= s"""val t${getId(node)} = Cycle($label)\n"""
      None

    case Star(children) =>
      children.foreach(visit(_))
      val label = "List(" + children.map(c => "t" + getId(c)).mkString(", ") + ")"
      sb ++= s"""val t${getId(node)} = Star($label)\n"""
      None

    case Plus(children) =>
      children.foreach(visit(_))
      val label = "List(" + children.map(c => "t" + getId(c)).mkString(", ") + ")"
      sb ++= s"""val t${getId(node)} = Plus($label)\n"""
      None

    case Group(children) =>
      children.foreach(visit(_))
      val label = "List(" + children.map(c => "t" + getId(c)).mkString(", ") + ")"
      sb ++= s"""val t${getId(node)} = Group($label)\n"""
      None

    case Opt(child) =>
      visit(child)
      sb ++= s"""val t${getId(node)} = Opt(t${getId(child)})\n"""
      None

    case Alt(children) =>
      children.foreach(visit(_))
      val label = "List(" + children.map(c => "t" + getId(c)).mkString(", ") + ")"
      sb ++= s"""val t${getId(node)} = Alt($label)\n"""
      None
  }
}
