
package iguana.parsetrees.term

import iguana.parsetrees.visitor._
import iguana.utils.visualization.GraphVizUtil._

import scala.collection.mutable.{Buffer, StringBuilder}

object TermVisualization {

  def generate(node: Term, dir: String, fileName: String) {
    val treeToDot = new TermToDot with Memoization[Term]
    treeToDot.visit(node)
    generateGraph(treeToDot.get, dir, fileName)
  }

  def generateWithoutLayout(node: Term, dir: String, fileName: String) {
    val treeToDot = new TermToDot with Memoization[Term] with Predicate[Term] {
      override def predicate: Term => Boolean = tree => tree match {
        case r: NonterminalTerm => !r.isLayout
        case t: TerminalTerm => !t.isLayout
        case _           => true
      }
    }
    treeToDot.visit(node)
    generateGraph(treeToDot.get, dir, fileName)
  }

}

class TermToDot extends Visitor[Term] with Id {

  type T = Seq[Int]

  def get: String = sb.toString

  val sb = new StringBuilder

  override def visit(node: Term): VisitResult[T] = node match {

    case TerminalTerm(name, i, j, input) =>
      val id = getId(node)
      sb ++= s"$id ${ROUNDED_RECTANGLE.format("black", escape(input.subString(i, j)))}\n"
      Some(Buffer(id))

    case NonterminalTerm(r, children, input) =>
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", r.head)}\n"
      val ids = for (c <- children; x <- visit(c).toSeq; i <- x) yield i
      addEdge(node, ids)

     case AmbiguityTerm(branches) =>
       sb ++= s"${getId(node)} ${DIAMOND.format("red")}\n"
       val ids: Seq[Int] = (for (b <- branches; x <- getBranch(b).toSeq; i <- x) yield i) (collection.breakOut)
       addEdge(node, ids)

    case Epsilon(i) =>
      val id = getId(node)
      sb ++= s"$id ${ROUNDED_RECTANGLE.format("black", "&epsilon;")}\n"
      Some(Buffer(id))

    case Cycle(label) =>
      val id = getId(node)
      sb ++= s"$id ${CIRCLE.format("red", label, "")}\n"
      Some(Buffer(id))

    case Star(children) =>
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", "*")}\n"
      val ids = for (c <- children; x <- visit(c).toSeq; i <- x) yield i
      addEdge(node, ids)

    case Plus(children) =>
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", "+")}\n"
      val ids = for (c <- children; x <- visit(c).toSeq; i <- x) yield i
      addEdge(node, ids)

    case Group(children) =>
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", "()")}\n"
      val ids = for (c <- children; x <- visit(c).toSeq; i <- x) yield i
      addEdge(node, ids)

    case Opt(child) =>
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", "?")}\n"
      val ids = for (v <- visit(child).toSeq; i <- v) yield i
      addEdge(node, ids)

    case Alt(children) =>
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", "|")}\n"
      val ids = for (c <- children; x <- visit(c).toSeq; i <- x) yield i
      addEdge(node, ids)

  }

  def getBranch(b: Seq[Term]): VisitResult[Seq[Int]] = {
    sb ++= s"${getId(b)} ${CIRCLE.format("black", "", "")}\n"
    val ids = for (c <- b; x <- visit(c).toSeq; i <- x) yield i
    addEdge(b, ids)
  }

  def addEdge(node: Any, dstIds: Seq[Int]): VisitResult[Seq[Int]] = {
    if (! dstIds.isEmpty)
      sb ++= s"edge [color=black, style=solid, penwidth=0.5, arrowsize=0.7]; ${getId(node)} -> { ${dstIds.mkString(", ")} }\n"
    Some(Buffer(getId(node)))
  }

  def escape(s: Any) = s.toString.replaceAll("\"", "\\\\\"")
                                 .replaceAll("\n", "n")
                                 .replaceAll("\t", "t")
                                 .replaceAll("\r", "r")

}
