/*
 * Copyright (c) 2015, Ali Afroozeh and Anastasia Izmaylova, Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this
 *    list of conditions and the following disclaimer in the documentation and/or
 *    other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 */

package iguana.parsetrees.sppf

import scala.collection.mutable._

object NonterminalNodeType extends Enumeration {
  type NonterminalNodeType = Value
  val Basic, Star, Plus, Opt, Seq = Value
}

object TerminalNodeType extends Enumeration {
  type TerminalNodeType = Value
  val Basic, Layout = Value
}

/**
 * Utility class for creating SPPF nodes from Java
 */
object SPPFNodeFactory {

  import NonterminalNodeType._

  def createTerminalNode(s: Any, leftExtent: Int, rightExtent: Int) = new BasicTerminalNode(s, leftExtent, rightExtent)

  def createEpsilonNode(s: Any, index: Int) = createTerminalNode(s, index, index)

  def createLayoutNode(s: Any, leftExtent: Int, rightExtent: Int) = new LayoutTerminalNode(s, leftExtent, rightExtent)

  def createNonterminalNode(head: Any, slot: Any, child: NonPackedNode) = NonterminalNode(head, slot, child, None)
  def createNonterminalNode(head: Any,
                            slot: Any,
                            child: NonPackedNode,
                            action: Option[Action] = None,
                            ruleType: Option[Any] = None,
                            nodeType: NonterminalNodeType = Basic) = NonterminalNode(head, slot, child, action, ruleType, nodeType)

  def createIntermediateNode(s: Any, leftChild: NonPackedNode, rightChild: NonPackedNode) = IntermediateNode(s, leftChild, rightChild)
}

trait SPPFNode {
	type T <: SPPFNode
  def children: Seq[T]
  def size: Int = children.size
  def leftExtent: Int
  def rightExtent: Int
  def slot: Any

  // For compatibility with Java code
  def getLeftExtent = leftExtent
  def getRightExtent = rightExtent
}

trait NonPackedNode extends SPPFNode {

  type T = PackedNode

  def isAmbiguous: Boolean
  def name: String = slot.toString
	override def toString  = name + "," + leftExtent + "," + rightExtent
}

class DummyNode extends NonPackedNode {
  def children = ListBuffer()
  def leftExtent = -1
  def rightExtent = -1
  def slot: Any = "$"
  def isAmbiguous = false
}

object DummyNode {
  val getInstance = new DummyNode
}

abstract class NonterminalOrIntermediateNode(child: PackedNode) extends NonPackedNode {
  protected var rest: ListBuffer[T] = null
  def children: Seq[T] = if (rest != null) ListBuffer(child) ++ rest else ListBuffer(child)
  def leftExtent: Int = child.leftExtent
  def rightExtent: Int = child.rightExtent
  def isAmbiguous = rest != null && !rest.isEmpty
}

abstract class NonterminalNode(val child: PackedNode) extends NonterminalOrIntermediateNode(child) {
  def getValue: Any = ???

  /**
   * @return true if the second packed node of this nonterminal node is added.
   *         This is useful for couting the number of ambigous nodes.
   */
  def addPackedNode(slot: Any, child: NonPackedNode) =
    if (rest == null) {
      rest = ListBuffer(PackedNode(slot, child))
      true
    } else {
      rest += PackedNode(slot, child)
      false
    }

  override def equals(obj: Any): Boolean = obj match {
    case NonterminalNode(slot, leftExtent, rightExtent, children) =>
      this.slot == slot &&
      this.leftExtent == leftExtent &&
      this.rightExtent == rightExtent &&
      this.children == children
    case _ => false
  }
}

abstract class NonterminalNodeWithValue(child: PackedNode, value: Any) extends NonterminalNode(child) {
  override def getValue = value
}

object NonterminalNode {

  import NonterminalNodeType._

  def apply(head: Any, slot: Any, child: NonPackedNode, value: Option[Any], ruleType: Option[Any] = None, nodeType: NonterminalNodeType = Basic) = nodeType match {
    case Basic => value match {
      case Some(v) => new BasicNonterminalNodeWithValue(head, PackedNode(slot, child), v)
      case None    => new BasicNonterminalNode(head, PackedNode(slot, child))
    }
    case Star => value match {
      case Some(v) => new StarNonterminalNodeWithValue(head, PackedNode(slot, child), v)
      case None    => new StarNonterminalNode(head, PackedNode(slot, child))
    }
    case Plus => value match {
      case Some(v) => new PlusNonterminalNodeWithValue(head, PackedNode(slot, child), v)
      case None    => new PlusNonterminalNode(head, PackedNode(slot, child))
    }
    case Opt => value match {
      case Some(v) => new OptNonterminalNodeWithValue(head, PackedNode(slot, child), v)
      case None    => new OptNonterminalNode(head, PackedNode(slot, child))
    }
    case Seq => value match {
      case Some(v) => new SeqNonterminalNodeWithValue(head, PackedNode(slot, child), v)
      case None    => new SeqNonterminalNode(head, PackedNode(slot, child))
    }
  }

  def unapply(n: NonterminalNode): Option[(Any, Int, Int, Seq[PackedNode])]
    = Some((n.slot,  n.child.leftExtent, n.children.last.rightExtent, n.children))
}

class BasicNonterminalNode(val slot: Any, child: PackedNode) extends NonterminalNode(child)
class BasicNonterminalNodeWithValue(val slot: Any, child: PackedNode, val value: Any) extends NonterminalNodeWithValue(child, value)
class StarNonterminalNode(val slot: Any, child: PackedNode) extends  NonterminalNode(child)
class StarNonterminalNodeWithValue(val slot: Any, child: PackedNode, val value: Any) extends  NonterminalNodeWithValue(child, value)
class PlusNonterminalNode(val slot: Any, child: PackedNode) extends NonterminalNode(child)
class PlusNonterminalNodeWithValue(val slot: Any, child: PackedNode, val value: Any) extends NonterminalNodeWithValue(child, value)
class OptNonterminalNode(val slot: Any, child: PackedNode) extends NonterminalNode(child)
class OptNonterminalNodeWithValue(val slot: Any, child: PackedNode, val value: Any) extends NonterminalNodeWithValue(child, value)
class SeqNonterminalNode(val slot: Any, child: PackedNode) extends NonterminalNode(child)
class SeqNonterminalNodeWithValue(val slot: Any, child: PackedNode, val value: Any) extends NonterminalNodeWithValue(child, value)

class IntermediateNode(val slot: Any, val child: PackedNode) extends NonterminalOrIntermediateNode(child) {

  def addPackedNode(slot: Any, leftChild: NonPackedNode, rightChild: NonPackedNode): Boolean =
    if (rest == null) {
      rest = ListBuffer(PackedNode(slot, leftChild, rightChild))
      true
    } else {
      rest += PackedNode(slot, leftChild, rightChild)
      false
    }

  override def equals(o: Any): Boolean = o match {
    case IntermediateNode(slot, leftExtent, rightExtent, children) =>
      slot == this.slot &&
      leftExtent == this.leftExtent &&
      rightExtent == this.rightExtent &&
      children == this.children

    case _ => false
  }
}

object IntermediateNode {
  def apply(slot: Any, leftChild: NonPackedNode, rightChild: NonPackedNode)
    = new IntermediateNode(slot, PackedNode(slot, leftChild, rightChild))

  def unapply(n: IntermediateNode): Option[(Any, Int, Int, Seq[PackedNode])]
    = Some((n.slot, n.child.leftExtent, n.child.rightExtent, n.children))
}

trait TerminalNode extends NonPackedNode {
  def isAmbiguous = false
  def children = ListBuffer()

  override def equals(o: Any): Boolean = o match {
    case TerminalNode(slot, leftExtent, rightExtent) =>
      slot == this.slot &&
      leftExtent == this.leftExtent &&
      rightExtent == this.rightExtent
    case _ => false
  }
}

object TerminalNode {

  import TerminalNodeType._

  def apply(slot: Any, leftExtent: Int, rightExtent: Int, nodeType: TerminalNodeType = Basic) = new BasicTerminalNode(slot, leftExtent, rightExtent)

  def unapply(n: TerminalNode): Option[(Any, Int, Int)] = Some((n.slot, n.leftExtent, n.rightExtent))
}

class BasicTerminalNode(val slot: Any, val leftExtent: Int, val rightExtent: Int) extends TerminalNode {
	def this(c:Char, inputIndex: Int) = this(c + "", inputIndex, inputIndex + 1)
}

class LayoutTerminalNode(val slot: Any, val leftExtent: Int, val rightExtent: Int) extends TerminalNode


trait PackedNode extends SPPFNode {
  type T = NonPackedNode
  def leftChild: T
  def rightChild: Option[T] = None

  def leftExtent = leftChild.leftExtent
  def rightExtent = if (rightChild.isDefined) rightChild.get.rightExtent else leftChild.rightExtent

  def pivot = leftChild.rightExtent
  def children: Seq[T] = if (rightChild.isDefined) ListBuffer(leftChild, rightChild.get) else ListBuffer(leftChild)
  def action: Option[Action] = None
  def rule: Option[Any] = None
  def hasRightChild: Boolean  = rightChild != null

  override def toString = slot + "," + pivot

  override def equals(o: Any) = o match {
    case PackedNode(slot, pivot, leftChild, rightChild) =>
      slot == this.slot &&
      pivot == this.pivot &&
      leftChild == this.leftChild &&
      rightChild == this.rightChild
    case _             => false
  }
}

object PackedNode {

  def apply(s: Any, child: NonPackedNode): PackedNode = new PackedNode { override def leftChild = child; override def slot = s }
  def apply(s: Any, child: NonPackedNode, a: Action): PackedNode = new PackedNode { override def leftChild = child; override def slot = s; override def action = Some(a) }
  def apply(s: Any, child: NonPackedNode, r: Any): PackedNode = new PackedNode { override def leftChild = child; override def slot = s; override def rule = Some(r) }
  def apply(s: Any, child: NonPackedNode, a: Action, r: Any): PackedNode = new PackedNode { override def leftChild = child; override def slot = s; override def action = Some(a); override def rule = Some(r) }

  def apply(s: Any, l: NonPackedNode, r: NonPackedNode): PackedNode = new PackedNode { override def leftChild = l; override def slot = s; override def rightChild = Some(r) }
  def apply(s: Any, l: NonPackedNode, r: NonPackedNode, a: Action): PackedNode = new PackedNode { override def leftChild = l; override def rightChild = Some(r); override def slot = s; override def action = Some(a) }
  def apply(s: Any, l: NonPackedNode, r: NonPackedNode, _r: Any): PackedNode = new PackedNode { override def leftChild = l; override def rightChild = Some(r); override def slot = s; override def rule = Some(_r) }
  def apply(s: Any, l: NonPackedNode, r: NonPackedNode, a: Action, _r: Any): PackedNode = new PackedNode { override def leftChild = l; override def rightChild = Some(r); override def slot = s; override def action = Some(a); override def rule = Some(_r) }

  def unapply(n: PackedNode): Option[(Any, Int, NonPackedNode, Option[NonPackedNode])]
    = Some((n.slot, n.pivot, n.leftChild, n.rightChild))
}