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

import iguana.parsetrees.slot._

import scala.collection.mutable.{ListBuffer, Set}


/**
 * Utility class for creating SPPF nodes from Java
 */
object SPPFNodeFactory {

  def createTerminalNode(s: Any, leftExtent: Int, rightExtent: Int) = new TerminalNode(s.asInstanceOf[TerminalSlot], leftExtent, rightExtent)

  def createEpsilonNode(s: Any, index: Int) = createTerminalNode(s, index, index)

  def createNonterminalNode(head: Any, slot: Any, child: NonPackedNode)
      = NonterminalNode(head.asInstanceOf[NonterminalSlot], slot.asInstanceOf[EndSlot], child)

  def createNonterminalNode(head: Any, slot: Any, child: NonPackedNode, value: Any)
      = NonterminalNode(head.asInstanceOf[NonterminalSlot], slot.asInstanceOf[EndSlot], child, value)

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

class NonterminalNode(val slot: NonterminalSlot, val child: PackedNode) extends NonterminalOrIntermediateNode(child) {

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
      case n:NonterminalNode =>
          this.slot == n.slot &&
          this.leftExtent == n.leftExtent &&
          this.rightExtent == n.rightExtent &&
          this.children.zip(n.children).forall {
            case (x, y) => x.deepEquals(y, visited) }
      case _ => false
    }
  }

}

object NonterminalNode {

  def apply(head: NonterminalSlot, slot: EndSlot, child: NonPackedNode): NonterminalNode
    = new NonterminalNode(head, PackedNode(slot, child, slot.action, slot.ruleType))

  def apply(head: NonterminalSlot, slot: EndSlot, child: NonPackedNode, value: Any): NonterminalNode = {

    val packedNode = PackedNode(slot, child, slot.action, slot.ruleType)

    if (value == null) new NonterminalNode(head, packedNode)
    else new NonterminalNode(head, packedNode) { override def getValue = value }

  }

  def unapply(n: NonterminalNode): Option[(NonterminalSlot, PackedNode)]
    = Some((n.slot,  n.child))

}

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

case class TerminalNode(val slot: TerminalSlot, val leftExtent: Int, val rightExtent: Int) extends NonPackedNode {
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