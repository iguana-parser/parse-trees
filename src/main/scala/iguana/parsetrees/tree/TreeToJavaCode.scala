
package iguana.parsetrees.tree

import iguana.parsetrees.visitor._
import iguana.utils.input.Input

import scala.collection.mutable.StringBuilder
import scala.collection.immutable.Set


object TreeToJavaCode {

  def get(node: Tree, input: Input): String = {
    val treeToJavaCode = new ToJavaCode(input) with Memoization[Tree]
    treeToJavaCode.visit(node)
    treeToJavaCode.get
  }

}

class ToJavaCode(val input: Input) extends Visitor[Tree] with Id {

  type T = Unit

  def get: String = sb.toString

  val sb = new StringBuilder

  override def visit(node: Tree): VisitResult[Unit] = node match {

    case Terminal(name, i, j) =>
      sb ++= s"""Tree t${getId(node)} = createTerminal($i, $j);\n"""
      None

    case RuleNode(r, children) =>
      children.foreach(visit(_))
      val label = "list(" + children.map(c => "t" + getId(c)).mkString(", ") + ")"
      sb ++= s"""Tree t${getId(node)} = createRule($r, $label);\n"""
      None

     case Amb(branches: Set[Branch[Tree]]) =>
       branches.foreach(b => b.children.foreach(c => visit(c)))
       val label = branches.map(b => "createBranch(list(" + b.children.map(c => "t" + getId(c)).mkString(", ") + "))").mkString(", ")
       sb ++= s"""Tree t${getId(node)} = createAmbiguity(set($label));\n"""
       None

    case Epsilon(i) =>
      sb ++= s"""Tree t${getId(node)} = createEpsilon($i);\n"""
      None

    case Cycle() =>
      sb ++= s"""Tree t${getId(node)} = createCycle();\n"""
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
