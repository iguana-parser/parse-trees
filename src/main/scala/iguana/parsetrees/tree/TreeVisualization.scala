
package iguana.parsetrees.tree

import iguana.parsetrees.visitor._
import iguana.utils.input.Input
import iguana.utils.visualization.GraphVizUtil._

import scala.collection.mutable.{Buffer, StringBuilder, Set}

object TreeVisualization {

  def generate(node: Tree, dir: String, fileName: String, input: Input) {
    val treeToDot = new TreeToDot(input) with Memoization[Tree]
    treeToDot.visit(node)
    generateGraph(treeToDot.get, dir, fileName)
  }

  def generate(node: Tree, dir: String, fileName: String, input: Input, ignore: java.util.Set[String]) {
    val treeToDot = new TreeToDot(input) with Memoization[Tree] with Predicate[Tree] {
      override def predicate: Tree => Boolean = t => t match {
        case RuleNode(r, ts) => !ignore.contains(r.head)
        case _ => true
      }
    }
    treeToDot.visit(node)
    generateGraph(treeToDot.get, dir, fileName)
  }

}

class TreeToDot(input: Input) extends Visitor[Tree] with Id {

  type T = Seq[Int]

  def get: String = sb.toString

  val sb = new StringBuilder

  override def visit(node: Tree): VisitResult[T] = node match {

    case Terminal(name, i, j) =>
      val id = getId(node)
      sb ++= s"$id ${ROUNDED_RECTANGLE.format("black", escape(input.subString(i, j)))}\n"
      Some(Buffer(id))

    case RuleNode(r, children) =>
      sb ++= s"${getId(node)} ${ROUNDED_RECTANGLE.format("black", r.head)}\n"
      val ids = for (c <- children; x <- visit(c).toSeq; i <- x) yield i
      addEdge(node, ids)

     case Amb(branches: Set[Branch[Tree]]) =>
       sb ++= s"${getId(node)} ${DIAMOND.format("red")}\n"
       val ids: Seq[Int] = (for (b <- branches; x <- getBranch(b).toSeq; i <- x) yield i) (collection.breakOut)
       addEdge(node, ids)

    case Epsilon(i) =>
      val id = getId(node)
      sb ++= s"$id ${ROUNDED_RECTANGLE.format("black", "&epsilon;")}\n"
      Some(Buffer(id))

    case Cycle(label) =>
      val id = getId(node)
      sb ++= s"$id ${CIRCLE.format("red", "Cycle", label)}\n"
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

  def getBranch(b: Branch[Tree]): VisitResult[Seq[Int]] = {
    sb ++= s"${getId(b)} ${CIRCLE.format("black", "", "")}\n"
    val ids = for (c <- b.children; x <- visit(c).toSeq; i <- x) yield i
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
