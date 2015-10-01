package iguana.parsetrees.sppf

import iguana.parsetrees.visitor.{Memoization, Id, Visitor}

object SPPFToJavaCode {

  def get(node: SPPFNode): String = {
    val toJavaCode = new ToJavaCode with Memoization[SPPFNode]
    toJavaCode.visit(node)
    toJavaCode.get
  }

}

class ToJavaCode extends Visitor[SPPFNode] with Id {

  override type T = Unit

  val sb = new StringBuilder

  def get = sb.toString

  def visit(node: SPPFNode): Option[T] = node match {

    case NonterminalNode(name, leftExtent, rightExtent, children) =>
      children.foreach(p => p.children.foreach(visit(_)))
      sb ++= s"""NonterminalNode node${getId(node)} = createNonterminalNode(registry.getSlot("$name"), registry.getSlot("${children.head.slot.toString}"), node${getId(children.head.leftChild)});\n"""
      children.tail.foreach(c => sb ++= s"""node${getId(node)}.addPackedNode(registry.getSlot("${c.slot.toString}"), node${getId(c.leftChild)});\n""")
      None

    case IntermediateNode(name, leftExtent, rightExtent, children) =>
      children.foreach(p => p.children.foreach(visit(_)))
      sb ++= s"""IntermediateNode node${getId(node)} = createIntermediateNode(registry.getSlot("$name"), node${getId(children.head.leftChild)}, node${getId(children.head.rightChild)});\n"""
      children.tail.foreach(c => sb ++= s"""node${getId(node)}.addPackedNode(registry.getSlot("${c.slot.toString}"), node${getId(c.leftChild)}, node${getId(c.rightChild)});\n""")
      None

    case TerminalNode(name, leftExtent, rightExtent) =>
      sb ++= s"""TerminalNode node${getId(node)} = createTerminalNode(registry.getSlot("$name"), $leftExtent, $rightExtent);\n"""
      None
  }
}