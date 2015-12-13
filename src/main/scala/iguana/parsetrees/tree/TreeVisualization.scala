
package iguana.parsetrees.tree

import iguana.parsetrees.visitor._
import iguana.utils.visualization.GraphVizUtil._

import scala.collection.mutable.{Buffer, StringBuilder}

object TreeVisualization {

  def generate(node: Tree, dir: String, fileName: String) {
    val treeToDot = new TreeToDot with Memoization[Tree]
    generateGraph(treeToDot.visit(node).get.toString, dir, fileName)
  }

  def generateWithoutLayout(node: Tree, dir: String, fileName: String) {
    val treeToDot = new TreeToDot with Memoization[Tree] with Predicate[Tree] {
      override def predicate: Tree => Boolean = tree => tree match {
        case r: RuleNode => {
          !r.isLayout
        }
        case t: Terminal => {
          !t.isLayout
        }
        case _           => true
      }
    }
    generateGraph(treeToDot.visit(node).get.toString, dir, fileName)
  }

}

class TreeToDot extends Visitor[Tree] with Id {

  type T = StringBuilder

  override def visit(node: Tree): VisitResult[T] = node match {

    case Terminal(name, i, j, input) =>
      Some(new StringBuilder(s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", escape(input.subString(i, j)))}\n"))

    case RuleNode(r, children, input) =>
      val sb = new scala.StringBuilder()
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", r)}\n"

      for (c <- children) {
        visit(c) match {
          case Some(s) => sb append s append addEdge(node, c)
          case _       =>
        }
      }
      Some(sb)

     case Amb(branches: Set[Branch[Tree]]) =>
       val sb = new StringBuilder
       sb ++= s"${getId(node)} ${DIAMOND.format("red")}\n"
       for (b <- branches) {
         for (c <- b.children) {
           sb ++= s"${getId(b)} ${CIRCLE.format("black", "", "")}\n"
           addEdge(b, c)
         }
       }
      Some(sb)

    case Epsilon(i) =>
      Some(new StringBuilder(s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", "&epsilon;")}\n"))

    case Cycle(label) =>
      Some(new StringBuilder(s"${getId(node)} ${CIRCLE.format("red", label, "")}\n"))

    case Star(children) =>
      val sb = new StringBuilder
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", "*")}\n"
      addEdge(node, children, sb)
      Some(sb)


    case Plus(children) =>
      val sb = new StringBuilder
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", "+")}\n"
      addEdge(node, children, sb)
      Some(sb)

    case Group(children) =>
      val sb = new StringBuilder
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", "()")}\n"
      addEdge(node, children, sb)
      Some(sb)

    case Opt(child) =>
      val sb = new StringBuilder
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", "?")}\n"
      addEdge(node, Buffer(child), sb)
      Some(sb)

    case Alt(children) =>
      val sb = new StringBuilder
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", "|")}\n"
      addEdge(node, children, sb)
      Some(sb)
  }


  def addEdge(node: Tree, children: Seq[Tree], sb: StringBuilder): Unit = {
    for (c <- children) {
      visit(c) match {
        case Some(s) => sb append s append addEdge(node, c)
        case _       =>
      }
    }
    Some(sb)
  }

  def addEdge(src: Any, dst: Any) = s"""edge [color=black, style=solid, penwidth=0.5, arrowsize=0.7]; ${getId(src)} -> { ${getId(dst)} }\n"""

  def escape(s: Any) = s.toString.replaceAll("\"", "\\\\\"")
                                 .replaceAll("\n", "n")
                                 .replaceAll("\t", "t")
                                 .replaceAll("\r", "r")

}
