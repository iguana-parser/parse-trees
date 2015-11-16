package iguana.parsetrees.tree

import iguana.parsetrees.slot.NonterminalNodeType
import iguana.parsetrees.sppf._
import iguana.parsetrees.visitor._

import scala.collection.mutable.Buffer

object TermBuilder {

  def build[T >: Any](node: SPPFNode, builder: TreeBuilder[T]): T
    = build(node, builder, new TermBuilderSPPFVisitor(builder) with SPPFMemoization);

  def build_no_memo[T >: Any](node: SPPFNode, builder: TreeBuilder[T]): T
    = build(node, builder, new TermBuilderSPPFVisitor(builder));

  private def build[T >: Any](node: SPPFNode, builder: TreeBuilder[T], visitor: TermBuilderSPPFVisitor): T =  {
    visitor.visit(node) match {
      case Some(v) => v.asInstanceOf[T]
      case None    => throw new RuntimeException()
    }
  }

}


class TermBuilderSPPFVisitor(builder: TreeBuilder[Any]) extends Visitor[SPPFNode] {

  override type T = Any

  case class PlusList(l: Buffer[T])

  case class EpsilonList(i: Int)

  override def visit(node: SPPFNode): VisitResult[T] = node match {

    case TerminalNode(slot, leftExtent, rightExtent, input) =>
      if (leftExtent == rightExtent) Some(EpsilonList(leftExtent))
      else
      if (slot.terminalName == null) Some(builder.terminalNode(leftExtent, rightExtent, input))
      else Some(builder.terminalNode(slot.terminalName, leftExtent, rightExtent, input))

    case n@NonterminalNode(slot, child, input) =>
      if (n.isAmbiguous) {
        Some(builder.ambiguityNode(n.children.map(p => builder.branch(p.rule, makeList(visit(p.leftChild)))), n.leftExtent, n.rightExtent))
      } else {
        n.slot.nodeType match {
          case NonterminalNodeType.Basic => Some(builder.nonterminalNode(child.rule, makeList(visit(child.leftChild)), n.leftExtent, n.rightExtent, input))
          case NonterminalNodeType.Star  => Some(flattenStar(visit(child.leftChild)))
          case NonterminalNodeType.Plus  => Some(flattenPlus(visit(child.leftChild)))
          case NonterminalNodeType.Opt   => Some(builder.opt(makeList(visit(child.leftChild)).head))
          case NonterminalNodeType.Seq   => Some(builder.group(makeList(visit(child.leftChild))))
          case NonterminalNodeType.Alt   => Some(builder.alt(makeList(visit(child.leftChild))))
        }
      }

    case IntermediateNode(slot, leftExtent, rightExtent, children) =>
      if (children.size > 1) // Ambiguous node
        Some(builder.ambiguityNode(children.map(p => builder.branch(p.rule, merge(p).get)), leftExtent, rightExtent))
      else
        merge(children.head)

    case PackedNode(slot, pivot, leftChild, rightChild) => throw new RuntimeException("Should not come here!")
  }

  def makeList(v: Any): Buffer[T] = v match {
    case Unknown(label) => Buffer(builder.cycle(label))
    case None => Buffer()
    case Some(EpsilonList(i)) => Buffer(builder.epsilon(i))
    case Some(PlusList(Buffer(PlusList(l), r@_*))) => Buffer(builder.plus(l ++ r))
    case Some(PlusList(l)) => Buffer(builder.plus(l))
    case Some(l: Buffer[T]) => l
    case Some(x) => Buffer(x)
  }

  def flattenStar(child: Any): Any = child match {
    // A* ::= epsilon
    case Some(EpsilonList(i)) => builder.star(Buffer())
    // A* ::= A+
    case Some(PlusList(l))    => builder.star(l)
    // For cases where A* is ambiguous
    case Some(a: Amb)         => a
  }

  def flattenPlus(child: Any): Any = child match {
    // A+ ::= A+ A
    case Some(Buffer(PlusList(l), r@_*)) => PlusList(l ++ r)

    // A+ ::= A
    case Some(l: Buffer[Any]) => PlusList(l)

    case Some(x) => PlusList(Buffer(x))
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


  def merge(x: Any, y: Any): Buffer[Any] = (x, y) match {
      case (None, None) => Buffer()
      case (None, y)    => Buffer(y)
      case (x, None)    => Buffer(x)
      case (PlusList(l), y)    => Buffer(PlusList(l :+ y))
      case (x, PlusList(l))    => merge(x, builder.plus(l))
      case (l: Buffer[Any], y) => l :+ y
      case (x, y)  => Buffer(x, y)
  }

}
