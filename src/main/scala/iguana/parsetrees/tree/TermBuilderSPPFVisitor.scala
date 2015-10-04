package iguana.parsetrees.tree

import iguana.parsetrees.sppf._
import iguana.parsetrees.visitor.{Memoization, Visitor}

import scala.collection.mutable.{Buffer, Set}

object TermBuilder {

  def build[T >: Any](node: SPPFNode, builder: TreeBuilder[T]): Any =  {
    val visitor = new TermBuilderSPPFVisitor(builder) with Memoization[SPPFNode]
    visitor.visit(node) match {
      case Some(v) => v
      case None    => throw new RuntimeException()
    }
  }
}


class TermBuilderSPPFVisitor(builder: TreeBuilder[Any]) extends Visitor[SPPFNode] {

  override type T = Any

  case class StarList(l: Buffer[T])
  case class PlusList(l: Buffer[T])
  case class OptList(l: Buffer[T])
  case class GroupList(l: Buffer[T])

  override def visit(node: SPPFNode): Option[T] = node match {

    case TerminalNode(slot, leftExtent, rightExtent) =>
      if (leftExtent == rightExtent) Some(builder.epsilon(leftExtent))
      else Some(builder.terminalNode(leftExtent, rightExtent))

    case n@BasicNonterminalNode(slot, child) =>
      if (n.isAmbiguous) {
        Some(builder.ambiguityNode(n.children.map(p => builder.branch(getP1(p))), n.leftExtent, n.rightExtent))
      } else {
        val p = n.children.head
        Some(builder.nonterminalNode(p.rule, getP1(p), n.leftExtent, n.rightExtent))
      }

    case n@StarNonterminalNode(slot, child) =>
      if (n.isAmbiguous) Some(builder.ambiguityNode(n.children.map(p => builder.branch(getP1(p))), n.leftExtent, n.rightExtent))
      else Some(StarList(makeList2(visit(child.leftChild))))

    case n@PlusNonterminalNode(slot, child) =>
      if (n.isAmbiguous) Some(builder.ambiguityNode(n.children.map(p => builder.branch(getP1(p))), n.leftExtent, n.rightExtent))
      else Some(PlusList(makeList2(visit(child.leftChild))))

    case n@OptNonterminalNode(slot, child) =>
      if (n.isAmbiguous) Some(builder.ambiguityNode(n.children.map(p => builder.branch(getP1(p))), n.leftExtent, n.rightExtent))
      else Some(OptList(makeList2(visit(child.leftChild))))

    case n@GroupNonterminalNode(slot, child) =>
      if (n.isAmbiguous) Some(builder.ambiguityNode(n.children.map(p => builder.branch(getP1(p))), n.leftExtent, n.rightExtent))
      else Some(GroupList(makeList2(visit(child.leftChild))))

    case IntermediateNode(slot, leftExtent, rightExtent, children) =>
      if (children.size > 1) // Ambiguous node
        Some(builder.ambiguityNode(children.map(n => builder.branch(getP2(n).get)), leftExtent, rightExtent))
      else
        getP2(children.head)

    case PackedNode(slot, pivot, leftChild, rightChild) => throw new RuntimeException("Should not come here!")
  }

  def getP1(p: PackedNode): Buffer[T] = makeList(visit(p.leftChild))

  def makeList(v: Any): Buffer[T] = v match {
      case null => Buffer(builder.cycle())
      case Some(null) => Buffer(builder.cycle())
      case None => Buffer()
      case Some(StarList(Buffer(PlusList(l), r@_*))) => Buffer(builder.star(l ++= r))
      case Some(PlusList(Buffer(PlusList(l), r@_*))) => Buffer(builder.plus(l ++= r))
      case Some(StarList(l))  => Buffer(builder.star(l))
      case Some(PlusList(l))  => Buffer(builder.plus(l))
      case Some(OptList(l))   => Buffer(builder.opt(l.head))
      case Some(l: Buffer[T]) => l
      case Some(x) => Buffer(x)
      case l: Buffer[T] => l
      case x => Buffer(x)
  }

  def makeList2(v: Any): Buffer[T] = v match {
      case null => Buffer(builder.cycle())
      case Some(null) => Buffer(builder.cycle())
      case None => Buffer()
      case Some(StarList(Buffer(PlusList(l), r@_*))) => Buffer(StarList(l ++= r))
      case Some(StarList(Buffer(StarList(l), r@_*))) => Buffer(StarList(l ++= r))
      case Some(PlusList(Buffer(PlusList(l), r@_*))) => Buffer(PlusList(l ++= r))
      case Some(l: Buffer[T]) => l
      case Some(x) => Buffer(x)
      case l: Buffer[T] => l
      case x => Buffer(x)
  }

  /**
   * Gets the children of a packed node under intermediate node
   */
  def getP2(p: PackedNode): Option[Buffer[Any]] =
      for { x <- visit(p.leftChild)
          y <- visit(p.rightChild) }
      yield merge(x, y)

  def merge(x: Any, y: Any): Buffer[Any] = (x, y) match {
      case (l: Buffer[Any], y) => l :+ y
      case (StarList(l), y)    => l :+ y
      case (PlusList(l), y)    => l :+ y
      case (null, null)        => Buffer(builder.cycle(), builder.cycle())
      case (null, y)           => Buffer(builder.cycle(), y)
      case (x, null)           => Buffer(x, builder.cycle())
      case _                   => Buffer(x, y)
    }

//  def flatten(ls: Buffer[Any]): Buffer[Any]= ls flatMap {
//    case t: Buffer[Any] =>  flatten(t)
//    case c => List(c)
//  }

//  def toSet(ls: Seq[Any]): Set[Branch[T]] = {
//    val setBuilder = Set.newBuilder[Branch[T]]
//    ls.foreach(e => { if (e.isInstanceOf[Seq[T]]) setBuilder += builder.branch(e.asInstanceOf[Seq[T]])
//                      else setBuilder += builder.branch(Buffer(e)) })
//    setBuilder.result()
//  }

}
