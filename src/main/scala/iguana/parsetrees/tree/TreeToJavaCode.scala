
package iguana.parsetrees.tree

import iguana.parsetrees.visitor.{Memoization, Id, Visitor}
import iguana.utils.input.Input
import iguana.utils.visualization.GraphVizUtil._

import scala.collection.mutable.{Set, StringBuilder}

object TreeToJavaCode {

  def get(node: Tree): String = {
    val treeToJavaCode = new ToJavaCode() with Memoization[Tree]
    treeToJavaCode.visit(node)
    treeToJavaCode.get
  }

}

class ToJavaCode() extends Visitor[Tree] with Id {

  type T = Unit

  def get: String = sb.toString

  val sb = new StringBuilder

  override def visit(node: Tree): Option[Unit] = node match {

    case Terminal(name) =>
      sb ++= s"""Tree t${getId(node)} = createTerminal("$name");\n"""
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

    case Epsilon() =>
      sb ++= s"""Tree t${getId(node)} = createEpsilon();\n"""
      None

    case Cycle() =>
      sb ++= s"""Tree t${getId(node)} = createCycle();\n"""
      None
  }
}
