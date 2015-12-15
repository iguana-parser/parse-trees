
package iguana.parsetrees.tree

import iguana.parsetrees.visitor._

import scala.collection.mutable.StringBuilder


object TreeToJavaCode {

  def get(node: Tree): String = {
    val treeToJavaCode = new ToJavaCode with Memoization[Tree]
    treeToJavaCode.visit(node)
    treeToJavaCode.get
  }

}

class ToJavaCode extends Visitor[Tree] with Id {

  type T = Unit

  def get: String = sb.toString

  val sb = new StringBuilder

  override def visit(node: Tree): VisitResult[Unit] = node match {

    case Terminal(slot, i, j, input) =>
      sb ++= s"""Tree t${getId(node)} = createTerminal($slot, $i, $j, input);\n"""
      None

    case RuleNode(r, children, input) =>
      children.foreach(visit(_))
      val label = "list(" + children.map(c => "t" + getId(c)).mkString(", ") + ")"
      sb ++= s"""Tree t${getId(node)} = createRule($r, $label, input);\n"""
      None

     case Amb(branches: Seq[Seq[Tree]]) =>
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
