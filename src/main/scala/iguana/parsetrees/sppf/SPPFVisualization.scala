package iguana.parsetrees.sppf

import iguana.parsetrees.slot.NonterminalNodeType
import iguana.parsetrees.visitor._

import iguana.utils.visualization.GraphVizUtil._

import scala.collection.mutable.Buffer

object SPPFVisualization {

  def generate(node: SPPFNode, dir: String, fileName: String) {
    val sppfToDot = new SPPFToDot with SPPFMemoization
    sppfToDot.visit(node)
    generateGraph(sppfToDot.get, dir, fileName)
  }

  def generateWithoutLayout(node: SPPFNode, dir: String, fileName: String) {

    val sppfToDot = new SPPFToDot with SPPFMemoization with Predicate[SPPFNode] {
      override def predicate: (SPPFNode) => Boolean = node => node match {
        case n:NonterminalNode => n.slot.nodeType != NonterminalNodeType.Layout
        case t:TerminalNode    => t.slot.terminalType.nodeType != NonterminalNodeType.Layout
        case _                 => true
      }
    }

    sppfToDot.visit(node)
    generateGraph(sppfToDot.get, dir, fileName)
  }
}


class SPPFToDot extends Visitor[SPPFNode] with Id {

  type T = Seq[Int]

  def get = sb.toString

  val sb = new StringBuilder

  def visit(node: SPPFNode): VisitResult[T] = node match {

      case n@NonterminalNode(slot, child, input) =>
        val color = if (n.isAmbiguous) "red" else "black"
        sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format(color, escape(slot) + "," + n.leftExtent + "," + n.rightExtent) + "\n"
        addEdge(n, for (c <- n.children; x <- visit(c).toSeq; y <- x) yield y)

      case n@IntermediateNode(name, leftExtent, rightExtent, children) =>
        val color = if (n.isAmbiguous) "red" else "black"
        sb ++= s"""${getId(node)}""" + RECTANGLE.format(color, escape(name) + "," + leftExtent + "," + rightExtent) + "\n"
        addEdge(n, for (c <- children; x <- visit(c).toSeq; i <- x) yield i)

      case n@TerminalNode(name, leftExtent, rightExtent, input) =>
          if (leftExtent == rightExtent)
            sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", "&epsilon;" + "," + leftExtent) + "\n"
          else
            sb ++= s"""${getId(node)}""" + ROUNDED_RECTANGLE.format("black", escape(name) + "," + leftExtent + "," + rightExtent + " (\\\"" + escape(input.subString(leftExtent, rightExtent)) +  "\\\")") + "\n"
      Some(Buffer(getId(n)))

      case n@PackedNode(name, pivot, leftChild, rightChild) =>
        sb ++= s"""${getId(node)}""" + CIRCLE.format("black", "","") + "\n"
        val left = getPackedNodeChild(leftChild)
        val right = getPackedNodeChild(rightChild)
        addEdge(n, left ++ right)
    }

  def getPackedNodeChild(n: NonPackedNode): Seq[Int] =
    if (n == null) Buffer()
    else visit(n) match {
        case Unknown(x) => Buffer(getId(x.asInstanceOf[NonterminalNode]))
        case Some(l)    => l
        case _          => Buffer()
    }

  //    if (n != null )
//      visit(n) match {
//      case Unknown(x) => Buffer(getId(x.asInstanceOf[NonterminalNode]))
//      case l => l.flatMap(id => addEdge(n, id)).toSeq.flatten
//    }
//    else Buffer()

  def addEdge(node: SPPFNode, childrenIds: Seq[Int]): VisitResult[Seq[Int]] = {
    if (!childrenIds.isEmpty)
      sb ++= s"edge [color=black, style=solid, penwidth=0.5, arrowsize=0.7]; ${getId(node)} -> { ${childrenIds.mkString(", ")} }\n"

    Some(Buffer(getId(node)))
  }

  def escape(s: Any) = s.toString.replaceAll("\"", "\\\\\"")
                                 .replaceAll("\n", "n")
                                 .replaceAll("\t", "t")
                                 .replaceAll("\r", "r")
}
