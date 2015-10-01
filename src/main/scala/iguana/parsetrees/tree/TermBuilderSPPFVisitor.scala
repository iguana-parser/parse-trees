package iguana.parsetrees.tree

import javafx.scene.shape.Circle

import iguana.parsetrees.sppf._
import iguana.parsetrees.visitor.{Memoization, Visitor}

import scala.collection.mutable
import scala.collection.mutable.{Buffer, ListBuffer, Set}

object TermBuilder {

  def build[T](node: SPPFNode, builder: TreeBuilder[T]): Any =  {
    val visitor = new TermBuilderSPPFVisitor[T](builder) with Memoization[SPPFNode]
    visitor.visit(node) match {
      case Some(v) => v
      case None    => throw new RuntimeException()
    }
  }

}

class TermBuilderSPPFVisitor[U](builder: TreeBuilder[U]) extends Visitor[SPPFNode] {

  override type T = Any

  override def visit(node: SPPFNode): Option[T] = node match {

    case TerminalNode(slot, leftExtent, rightExtent) =>
      if (leftExtent == rightExtent) Some(builder.epsilon(leftExtent))
      else Some(builder.terminalNode(leftExtent, rightExtent))

    case NonterminalNode(slot, leftExtent, rightExtent, children) =>
      if (children.size > 1) { // Ambiguous node
        Some(builder.ambiguityNode(toSet(children.flatMap(p => getP1(p))), leftExtent, rightExtent))
      } else {
        val p = children.head
        Some(builder.nonterminalNode(p.rule, makeList(getP1(p)), leftExtent, rightExtent))
      }

    case IntermediateNode(slot, leftExtent, rightExtent, children) =>
      if (children.size > 1) // Ambiguous node
        Some(builder.ambiguityNode(toSet(children.flatMap(n => getP2(n))), leftExtent, rightExtent))
      else
        getP2(children.head)

    case PackedNode(slot, pivot, leftChild, rightChild) => throw new RuntimeException("Should not come here!")

  }

  def getP1(p: PackedNode): Option[Any] = visit(p.leftChild) match {
    case Some(null) => Some(builder.cycle())
    case x          => x
  }
  
  /**
   * Gets the children of a packed node under intermediate node
   */
  def getP2(p: PackedNode): Option[Buffer[Any]] =
    for { x <- visit(p.leftChild)
          y <- visit(p.rightChild) }
      yield merge(x, y)

  def merge(x: Any, y: Any): Buffer[Any] = (x, y) match {
    case (l: Buffer[Any], y) => flatten(l) += y
    case (null, null)        => Buffer(builder.cycle(), builder.cycle())
    case (null, y)           => Buffer(builder.cycle(), y)
    case (x, null)           => Buffer(x, builder.cycle())
    case _                   => Buffer(x, y)
  }

  def makeList(v: Any): Buffer[U] = v match {
    case null => Buffer(builder.cycle())
    case None => Buffer()
    case Some(l: Buffer[U]) => l
    case Some(x) => ListBuffer(x.asInstanceOf[U])
  }

  def flatten(ls: Buffer[Any]): Buffer[Any]= ls flatMap {
    case t: Buffer[Any] =>  flatten(t)
    case c => List(c)
  }

  def toSet(ls: Seq[Any]): Set[Branch[U]] = {
    val setBuilder = Set.newBuilder[Branch[U]]
    ls.foreach(e => { if (e.isInstanceOf[Seq[U]]) setBuilder += builder.branch(e.asInstanceOf[Seq[U]])
                      else setBuilder += builder.branch(Buffer(e.asInstanceOf[U])) })
    setBuilder.result()
  }

}
