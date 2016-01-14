package iguana.parsetrees.term

import iguana.parsetrees.slot.NonterminalNodeType
import iguana.parsetrees.sppf._
import iguana.parsetrees.visitor._

import scala.annotation.tailrec
import scala.collection.mutable.Buffer

object SPPFToTerms {

  def convert[T](node: SPPFNode, builder: TermBuilder[T]): T =
    convert(node, builder, new TermBuilderSPPFVisitor(builder) with SPPFMemoization)

  def convertNoSharing[T](node: SPPFNode, builder: TermBuilder[T]): T =
    convert(node, builder, new TermBuilderSPPFVisitor(builder))

  private def convert[T](node: SPPFNode, builder: TermBuilder[T], visitor: TermBuilderSPPFVisitor[T]): T =  {
    visitor.visit(node) match {
      case Some(v) => v.asInstanceOf[T]
      case None    => throw new RuntimeException
    }
  }
}

class TermBuilderSPPFVisitor[U](builder: TermBuilder[U]) extends Visitor[SPPFNode] {

  override type T = Any

  case class PlusList(l: Buffer[T])

  case class EpsilonList(i: Int)

  var root = true

  override def visit(node: SPPFNode): VisitResult[T] = {

    val root = this.root
    this.root = false

    node match {

      case TerminalNode(slot, leftExtent, rightExtent, input) =>
        if (leftExtent == rightExtent) Some(EpsilonList(leftExtent))
        else Some(builder.terminalTerm(slot.terminalType, leftExtent, rightExtent, input))

      case n@NonterminalNode(slot, child, input) =>
        if (n.isAmbiguous) {
          Some(builder.ambiguityTerm(n.children.map(p => builder.nonterminalAmbiguityBranch(p.rule, makeList(visit(p.leftChild)).asInstanceOf[Seq[U]], input))))
        } else {
          n.slot.nodeType match {
            case NonterminalNodeType.Layout |
                 NonterminalNodeType.Basic => Some(builder.nonterminalTerm(child.rule, makeList(visit(child.leftChild)).asInstanceOf[Seq[U]], n.leftExtent, n.rightExtent, input))
            case NonterminalNodeType.Star  => Some(flattenStar(visit(child.leftChild)))
            case NonterminalNodeType.Plus  => Some(flattenPlus(visit(child.leftChild), root))
            case NonterminalNodeType.Opt   => Some(builder.opt(makeList(visit(child.leftChild)).head.asInstanceOf[U]))
            case NonterminalNodeType.Seq   => Some(builder.group(makeList(visit(child.leftChild)).asInstanceOf[Seq[U]]))
            case NonterminalNodeType.Alt   => Some(builder.alt(makeList(visit(child.leftChild)).asInstanceOf[Seq[U]]))
          }
        }

      case IntermediateNode(slot, leftExtent, rightExtent, children) =>
        if (children.size > 1) // Ambiguous node
          Some(builder.ambiguityTerm(children.map(p => builder.intermediateAmbiguityBranch(merge(p).get.asInstanceOf[Seq[U]]))))
        else
          merge(children.head)

      case PackedNode(slot, pivot, leftChild, rightChild) => throw new RuntimeException("Should not come here!")

    }
  }

  def makeList(v: Any): Buffer[T] = v match {
    case Unknown(node) => Buffer(builder.cycle(node))
    case None => Buffer()
    case Some(EpsilonList(i)) => Buffer(builder.epsilon(i))
    case Some(PlusList(Buffer(PlusList(l), r@_*))) => Buffer(builder.plus((l ++ r).asInstanceOf[Seq[U]]))
    case Some(PlusList(l)) => Buffer(builder.plus(l.asInstanceOf[Seq[U]]))
    case Some(l: Buffer[T]) => l
    case Some(x) => Buffer(x)
  }

  def flattenStar(child: Any): Any = child match {
    // A* ::= epsilon
    case Some(EpsilonList(i)) => builder.star(Buffer())
    // A* ::= A+
    case Some(PlusList(l))    => builder.star(l.asInstanceOf[Seq[U]])
    // For cases where A* is ambiguous
    case Some(a: AmbiguityTerm)         => a
  }

  def flattenPlus(child: Any, root: Boolean = false): Any = child match {
    // A+ ::= A+ A
    case Some(Buffer(PlusList(l), r@_*)) => if (root) builder.plus((l ++ r).asInstanceOf[Seq[U]]) else PlusList(l ++ r)

    // A+ ::= A
    case Some(l: Buffer[Any]) => if (root) builder.plus(l.asInstanceOf[Seq[U]]) else PlusList(l)

    case Some(x) => if (root) builder.plus(Buffer(x).asInstanceOf[Seq[U]]) else PlusList(Buffer(x))
  }

  /**
   * Gets the children of a packed node under intermediate node
   */
  def merge(p: PackedNode): VisitResult[Buffer[Any]] = {
    val x = visit(p.leftChild)
    val y = visit(p.rightChild)

    val left = x match {
      case None       => None
      case Unknown(l) => builder.cycle(l)
      case Some(v)    => v
    }

    val right = y match {
      case None       => None
      case Unknown(l) => builder.cycle(l)
      case Some(v)    => v
    }

    Some(merge(left, right))
  }

  @tailrec
  private def merge(x: Any, y: Any): Buffer[Any] = (x, y) match {
      case (None, None) => Buffer()
      case (None, y)    => Buffer(y)
      case (x, None)    => Buffer(x)
      case (PlusList(l), y)    => Buffer(PlusList(l :+ y))
      case (x, PlusList(l))    => merge(x, builder.plus(l.asInstanceOf[Seq[U]]))
      case (l: Buffer[Any], y) => l :+ y
      case (x, y)  => Buffer(x, y)
  }

}
