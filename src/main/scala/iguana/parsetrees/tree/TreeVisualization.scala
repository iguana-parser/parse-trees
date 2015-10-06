
package iguana.parsetrees.tree

import iguana.parsetrees.visitor.{Memoization, Id, Visitor}
import iguana.utils.input.Input
import iguana.utils.visualization.GraphVizUtil._

import scala.collection.mutable.StringBuilder
import scala.collection.immutable.Set

object TreeVisualization {

  def generate(node: Tree, dir: String, fileName: String, input: Input) {
    val treeToDot = new TreeToDot(input) with Memoization[Tree]
    treeToDot.visit(node)
    generateGraph(treeToDot.get, dir, fileName)
  }

}

class TreeToDot(input: Input) extends Visitor[Tree] with Id {

  type T = Unit

  def get: String = sb.toString

  val sb = new StringBuilder

  override def visit(node: Tree): Option[Unit] = node match {

    case Terminal(name, i, j) =>
      sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", input.subString(i, j)) + "\n"
      None

    case RuleNode(r, children) =>
      sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", r) + "\n"
      children.foreach(c => { visit(c); addEdge(node, c)})
      None

     case Amb(branches: Set[Branch[Tree]]) =>
       sb ++= s"""${getId(node)}""" + DIAMOND.format("red") + "\n"
       branches.foreach(b => { getBranch(b); addEdge(node, b)})
       None

    case Epsilon(i) =>
      sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", "&epsilon;") + "\n"
      None

    case Cycle() =>
      sb ++= s"""${getId(node)}""" + CIRCLE.format("red", "Cycle", "") + "\n"
      None

    case Star(children) =>
      sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", "*") + "\n"
      children.foreach(c => { visit(c); addEdge(node, c)})
      None

    case Plus(children) =>
      sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", "+") + "\n"
      children.foreach(c => { visit(c); addEdge(node, c)})
      None

    case Group(children) =>
      sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", "()") + "\n"
      children.foreach(c => { visit(c); addEdge(node, c)})
      None

    case Opt(child) =>
      sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", "?") + "\n"
      visit(child)
      addEdge(node, child)
      None

    case Alt(children) =>
      sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", "|") + "\n"
      children.foreach(c => { visit(c); addEdge(node, c)})
      None

  }

  def getBranch(b: Branch[Tree]): Unit = {
    sb ++= s"""${getId(b)}""" + CIRCLE.format("black", "", "") + "\n"
    b.children.foreach(t => { visit(t); addEdge(b, t) } )
  }

  def addEdge(src: Any, dst: Any) {
    sb ++= s"""edge [color=black, style=solid, penwidth=0.5, arrowsize=0.7]; ${getId(src)} -> { ${getId(dst)} }\n"""
  }
}
