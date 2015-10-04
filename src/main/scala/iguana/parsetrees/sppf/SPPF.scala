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

import scala.collection.mutable.{ListBuffer, Set}

trait Action {
  def apply(a: Any): Any
}

object NonterminalNodeType {

  type NonterminalNodeType = Int

  val Basic = 0
  val Star  = 1
  val Plus  = 2
  val Opt   = 3
  val Seq   = 4
}

object TerminalNodeType {

  type TerminalNodeType = Int

  val Basic = 0
  val Layout = 1
}

/**
 * Utility class for creating SPPF nodes from Java
 */
object SPPFNodeFactory {

  import NonterminalNodeType._

  def createTerminalNode(s: Any, leftExtent: Int, rightExtent: Int) = new BasicTerminalNode(s, leftExtent, rightExtent)

  def createEpsilonNode(s: Any, index: Int) = createTerminalNode(s, index, index)

  def createLayoutNode(s: Any, leftExtent: Int, rightExtent: Int) = new LayoutTerminalNode(s, leftExtent, rightExtent)

  def createNonterminalNode(head: Any, slot: Any, child: NonPackedNode, value: Any)
      = NonterminalNode(head, slot, child, Basic, null, null, null)

  def createNonterminalNode(head: Any, slot: Any, child: NonPackedNode)
  = NonterminalNode(head, slot, child, Basic, null, null)

  def createNonterminalNode(head: Any, slot: Any, child: NonPackedNode, nodeType: NonterminalNodeType, value: Any, action: Action, ruleType: Any)
      = NonterminalNode(head, slot, child, nodeType, value, action, ruleType)

  def createNonterminalNode(head: Any, slot: Any, child: NonPackedNode, nodeType: NonterminalNodeType, action: Action, ruleType: Any)
      = NonterminalNode(head, slot, child, nodeType, action, ruleType)

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

  def deepEquals(other: SPPFNode): Boolean = deepEquals(other, Set())
  def deepEquals(other: SPPFNode, visited: Set[SPPFNode]): Boolean
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

  override def deepEquals(other: SPPFNode, visited: Set[SPPFNode]): Boolean = other == DummyNode.getInstance
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

  def getValue: Any = null

  def addPackedNode(slot: Any, child: NonPackedNode): Boolean = addPackedNode(slot, child, null, null)

  /**
   * @return true if the second packed node of this nonterminal node is added.
   *         This is useful for couting the number of ambigous nodes.
   */
  def addPackedNode(slot: Any, child: NonPackedNode, action: Action, ruleType: Any) =
    if (rest == null) {
      rest = ListBuffer(PackedNode(slot, child, action, ruleType))
      true
    } else {
      rest += PackedNode(slot, child, action, ruleType)
      false
    }

  def deepEquals(other: SPPFNode, visited: Set[SPPFNode]): Boolean = {
    if (visited.contains(this)) return true else visited += this
    other match {
      case NonterminalNode(slot, leftExtent, rightExtent, children) =>
        this.slot == slot &&
          this.leftExtent == leftExtent &&
          this.rightExtent == rightExtent &&
          this.children.zip(children).forall {
            case (x, y) => x.deepEquals(y, visited) }
      case _ => false
    }
  }
}

object NonterminalNode {

  import NonterminalNodeType._

  def apply(head: Any, slot: Any, child: NonPackedNode, nodeType: NonterminalNodeType, _action: Action, _ruleType: Any) = {

    val packedNode = PackedNode(slot, child, _action, _ruleType)

    nodeType match {
      case Basic => new BasicNonterminalNode(head, packedNode)
      case Star  => new StarNonterminalNode(head, packedNode)
      case Plus  => new PlusNonterminalNode(head, packedNode)
      case Opt   => new OptNonterminalNode(head, packedNode)
      case Seq   => new GroupNonterminalNode(head, packedNode)
    }
  }

  def apply(head: Any, slot: Any, child: NonPackedNode, nodeType: NonterminalNodeType, _value: Any, _action: Action, _ruleType: Any) = {

    val packedNode = PackedNode(slot, child, _action, _ruleType)

    nodeType match {

      case Basic => if (_value == null) new BasicNonterminalNode(head, packedNode)
                    else new BasicNonterminalNode(head, packedNode) { override def getValue = _value }

      case Star  => if (_value == null) new StarNonterminalNode(head, packedNode)
                    else new StarNonterminalNode(head, packedNode) { override def getValue = _value }

      case Plus  => if (_value == null) new PlusNonterminalNode(head, packedNode)
                    else new PlusNonterminalNode(head, packedNode) { override def getValue = _value }

      case Opt  => if (_value == null) new OptNonterminalNode(head, packedNode)
                    else new OptNonterminalNode(head, packedNode) { override def getValue = _value }

      case Seq  => if (_value == null) new GroupNonterminalNode(head, packedNode)
                    else new GroupNonterminalNode(head, packedNode) { override def getValue = _value}
    }
  }

  def unapply(n: NonterminalNode): Option[(Any, Int, Int, Seq[PackedNode])]
    = Some((n.slot,  n.child.leftExtent, n.children.last.rightExtent, n.children))
}

case class BasicNonterminalNode(val slot: Any, override val child: PackedNode) extends NonterminalNode(child)
case class StarNonterminalNode(val slot: Any, override val child: PackedNode) extends  NonterminalNode(child)
case class PlusNonterminalNode(val slot: Any, override val child: PackedNode) extends NonterminalNode(child)
case class OptNonterminalNode(val slot: Any, override val child: PackedNode) extends NonterminalNode(child)
case class GroupNonterminalNode(val slot: Any, override val child: PackedNode) extends NonterminalNode(child)



class IntermediateNode(val slot: Any, val child: PackedNode) extends NonterminalOrIntermediateNode(child) {

  def addPackedNode(slot: Any, leftChild: NonPackedNode, rightChild: NonPackedNode): Boolean =
    if (rest == null) {
      rest = ListBuffer(PackedNode(slot, leftChild, rightChild))
      true
    } else {
      rest += PackedNode(slot, leftChild, rightChild, null, null)
      false
    }

  def deepEquals(other: SPPFNode, visited: Set[SPPFNode]): Boolean = {
    if (visited.contains(this)) return true else visited += this
    other match {
      case IntermediateNode(slot, leftExtent, rightExtent, children) =>
        slot == this.slot &&
          leftExtent == this.leftExtent &&
          rightExtent == this.rightExtent &&
          children.zip(this.children).forall { case (x, y) => x.deepEquals(y, visited) }
      case _ => false
    }
  }
}

object IntermediateNode {
  def apply(slot: Any, leftChild: NonPackedNode, rightChild: NonPackedNode)
    = new IntermediateNode(slot, PackedNode(slot, leftChild, rightChild, null, null))

  def unapply(n: IntermediateNode): Option[(Any, Int, Int, Seq[PackedNode])]
    = Some((n.slot, n.child.leftExtent, n.child.rightExtent, n.children))
}

trait TerminalNode extends NonPackedNode {
  def isAmbiguous = false
  def children = ListBuffer()

  def deepEquals(other: SPPFNode, visited: Set[SPPFNode]): Boolean = other match {
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
  def rightChild: T = null
  def action: Action = null
  def rule: Any = null

  def leftExtent = leftChild.leftExtent
  def rightExtent = if (rightChild != null) rightChild.rightExtent else leftChild.rightExtent
  def pivot = leftChild.rightExtent
  def children: Seq[T] = if (rightChild != null) ListBuffer(leftChild, rightChild) else ListBuffer(leftChild)
  def hasRightChild: Boolean  = rightChild != null

  override def toString = slot + "," + pivot

  def deepEquals(other: SPPFNode, visited: Set[SPPFNode]): Boolean = other match {
    case PackedNode(slot, pivot, leftChild, rightChild) =>
      slot == this.slot &&
      pivot == this.pivot &&
      leftChild.deepEquals(this.leftChild, visited) &&
      (if(rightChild != null) rightChild.deepEquals(this.rightChild, visited) else true)
    case _             => false
  }
}

object PackedNode {

  def apply(s: Any, l: NonPackedNode, r: NonPackedNode): PackedNode = new PackedNode {
    override def leftChild = l
    override def rightChild = r
    override def slot = s
  }

  def apply(s: Any, child: NonPackedNode, a: Action, r: Any): PackedNode =
      if (a == null && r == null) new PackedNode { override def leftChild = child; override def slot = s }
      else if (r == null) new PackedNode { override def leftChild = child; override def slot = s; override def action = a }
      else if (a == null) new PackedNode { override def leftChild = child; override def slot = s; override def rule = r }
      else new PackedNode { override def leftChild = child; override def slot = s; override def action = a; override def rule = r }

  def apply(s: Any, l: NonPackedNode, r: NonPackedNode, a: Action, _r: Any): PackedNode =
      if (a == null && r == null) new PackedNode { override def leftChild = l; override def slot = s; override def rightChild = r }
      else if (r == null) new PackedNode { override def leftChild = l; override def slot = s; override def action = a }
      else if (a == null) new PackedNode { override def leftChild = l; override def rightChild = r; override def slot = s }
      else new PackedNode { override def leftChild = l; override def rightChild = r; override def slot = s; override def action = a }

  def unapply(n: PackedNode): Option[(Any, Int, NonPackedNode, NonPackedNode)] = Some((n.slot, n.pivot, n.leftChild, n.rightChild))

}