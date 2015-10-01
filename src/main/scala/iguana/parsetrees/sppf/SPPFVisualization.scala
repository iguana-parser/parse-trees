package iguana.parsetrees.sppf

import iguana.parsetrees.visitor.{Memoization, Id, Visitor}
import iguana.utils.input.Input
import iguana.utils.visualization.GraphVizUtil

import scala.collection.mutable._
import iguana.utils.visualization.GraphVizUtil._

object SPPFVisualization {

  def generate(node: SPPFNode, dir: String, fileName: String, input: Input) {
    val sppfToDot = new SPPFToDot(input) with Memoization[SPPFNode]
    sppfToDot.visit(node)
    generateGraph(sppfToDot.get, dir, fileName)
  }
}

class SPPFToDot(input: Input) extends Visitor[SPPFNode] with Id {

  type T = Unit

  def get: String = sb.toString

  val sb = new StringBuilder

  def visit(node: SPPFNode): Option[T] = node match {

      case n@NonterminalNode(name, leftExtent, rightExtent, children) =>
        val color = if (n.isAmbiguous) "red" else "black"
        sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format(color, name + "," + leftExtent + "," + rightExtent) + "\n"
        children.foreach(c => { visit(c); addEdge(node, c, sb)})
        None

      case n@IntermediateNode(name, leftExtent, rightExtent, children) =>
        val color = if (n.isAmbiguous) "red" else "black"
        sb ++= s"""${getId(node)}""" + RECTANGLE.format(color, name + "," + leftExtent + "," + rightExtent) + "\n"
        children.foreach(c => { visit(c); addEdge(node, c, sb)})
        None

      case TerminalNode(name, leftExtent, rightExtent) =>
          if (leftExtent == rightExtent)
            sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", "&epsilon;" + "," + leftExtent) + "\n"
          else
            sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", name + "," + leftExtent + "," + rightExtent + " (\\\"" + input.subString(leftExtent, rightExtent) +  "\\\")") + "\n"
          None

      case PackedNode(name, pivot, leftChild, rightChild) =>
        sb ++= s"""${getId(node)}""" + CIRCLE.format("black", "", name + "," + pivot) + "\n"
        visit(leftChild);
        addEdge(node, leftChild, sb)

        if (rightChild != null) {
          visit(rightChild)
          addEdge(node, rightChild, sb)
        }
        None
    }

  def addEdge(src: SPPFNode, dst: SPPFNode, sb: StringBuilder) {
    sb ++= s"""edge [color=black, style=solid, penwidth=0.5, arrowsize=0.7]; ${getId(src)} -> { ${getId(dst)} }\n"""
  }

  def escape(s: Any): String = s.toString.replaceAll("\"", "\\\\\"").replaceAll("\t", "t").replaceAll("\n", "n").replaceAll("\r", "r")

}
