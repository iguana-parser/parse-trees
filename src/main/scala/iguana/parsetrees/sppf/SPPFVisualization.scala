package iguana.parsetrees.sppf

import iguana.parsetrees.slot.NonterminalNodeType
import iguana.parsetrees.visitor._

import scala.collection.mutable._
import iguana.utils.visualization.GraphVizUtil._

object SPPFVisualization {

  def generate(node: SPPFNode, dir: String, fileName: String) {
    val sppfToDot = new SPPFToDot with SPPFMemoization
    generateGraph(sppfToDot.visit(node).get.toString, dir, fileName)
  }

  def generateWithoutLayout(node: SPPFNode, dir: String, fileName: String) {

    val sppfToDot = new SPPFToDot with SPPFMemoization with Predicate[SPPFNode] {
      override def predicate: (SPPFNode) => Boolean = node => node match {
        case n:NonterminalNode => n.slot.nodeType != NonterminalNodeType.Layout
        case t:TerminalNode    => t.slot.terminalType.nodeType != NonterminalNodeType.Layout
        case _                 => true
      }
    }

    generateGraph(sppfToDot.visit(node).get.toString, dir, fileName)
  }
}

class SPPFToDot extends Visitor[SPPFNode] with Id {

  type T = StringBuilder

  def visit(node: SPPFNode): VisitResult[T] = node match {

      case n:NonterminalNode =>
        val children = n.children
        val name = n.name
        val slot = n.slot
        val color = if (n.isAmbiguous) "red" else "black"

        val sb = new StringBuilder
        sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format(color, escape(name) + "," + n.leftExtent + "," + n.rightExtent) + "\n"

        for (c <- children) {
          visit(c) match {
            case Some(s) => sb append s append addEdge(node, c)
            case _       =>
          }
        }

        Some(sb)

      case n@IntermediateNode(name, leftExtent, rightExtent, children) =>
        val color = if (n.isAmbiguous) "red" else "black"
        val sb = new StringBuilder
        sb ++= s"""${getId(node)}""" + RECTANGLE.format(color, escape(name) + "," + leftExtent + "," + rightExtent) + "\n"

        for (c <- children) {
          visit(c) match {
            case Some(s) => sb append s append addEdge(node, c)
            case _       =>
          }
        }

        Some(sb)

      case TerminalNode(name, leftExtent, rightExtent, input) =>
          if (leftExtent == rightExtent)
            Some(new StringBuilder(s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", "&epsilon;" + "," + leftExtent) + "\n"))
          else
            Some(new StringBuilder(s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", escape(name) + "," + leftExtent + "," + rightExtent + " (\\\"" + escape(input.subString(leftExtent, rightExtent)) +  "\\\")") + "\n"))

      case PackedNode(name, pivot, leftChild, rightChild) =>
        val sb = new scala.StringBuilder()
        sb ++= s"""${getId(node)}""" + CIRCLE.format("black", "","") + "\n"

        visit(leftChild) match {
          case Some(s) => sb append s append addEdge(node, leftChild)
          case _       =>
        }

        if (rightChild != null) {

          visit(rightChild) match {
            case Some(s) => sb append s append addEdge(node, rightChild)
            case _       =>
          }
        }
        Some(sb)
    }

  def addEdge(src: SPPFNode, dst: SPPFNode) = s"""edge [color=black, style=solid, penwidth=0.5, arrowsize=0.7]; ${getId(src)} -> { ${getId(dst)} }\n"""

  def escape(s: Any) = s.toString.replaceAll("\"", "\\\\\"")
                                 .replaceAll("\n", "n")
                                 .replaceAll("\t", "t")
                                 .replaceAll("\r", "r")
}
