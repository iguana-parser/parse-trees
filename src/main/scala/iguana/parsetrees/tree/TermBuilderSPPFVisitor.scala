package iguana.parsetrees.tree

import iguana.parsetrees.sppf._

import scala.collection.mutable._

object TermBuilder {

  def build[T](node: SPPFNode, builder: TreeBuilder[T]): Any =  {
    val visitor = new TermBuilderSPPFVisitor[T](builder)
    visitor.visit(node) match {
      case Some(v) => v
      case None    => throw new RuntimeException()
    }
  }

}

class TermBuilderSPPFVisitor[U](builder: TreeBuilder[U]) extends SPPFVisitor {

  override type T = Any

  override def visit(node: SPPFNode): Option[T] = node match {

    case TerminalNode(slot, leftExtent, rightExtent) =>
      if (leftExtent == rightExtent) None
      else Some(builder.terminalNode(leftExtent, rightExtent))

    case NonterminalNode(slot, leftExtent, rightExtent, children) =>
      if (children.size > 1) None
      else {
        val p = children.head
        Some(builder.nonterminalNode(p.rule, makeList(visit(p.leftChild)), leftExtent, rightExtent))
      }

    case IntermediateNode(slot, leftExtent, rightExtent, children) =>
      if (children.size > 1) None
      else {
        val p = children.head
        for (x <- visit(p.leftChild); y <- visit(p.rightChild)) yield merge(x, y)
      }

    case PackedNode(slot, pivot, leftChild, rightChild) => throw new RuntimeException("Should not come here!")
  }

  def merge(x: Any, y: Any) = x match {
    case l: Buffer[Any] => flatten(l) += y
    case x              => ListBuffer(x, y)
  }

  def makeList(v: Any): Buffer[U] = v match {
    case None => ListBuffer()
    case Some(l: Buffer[U]) => l
    case Some(x) => ListBuffer(x.asInstanceOf[U])
  }

  def flatten(ls: Buffer[Any]): Buffer[Any]= ls flatMap {
    case t: Buffer[Any] =>  flatten(t)
    case c => List(c)
  }

}
