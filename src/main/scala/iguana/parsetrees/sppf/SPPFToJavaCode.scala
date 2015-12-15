package iguana.parsetrees.sppf

import iguana.parsetrees.visitor._

object SPPFToJavaCode {

  def get(node: SPPFNode): String = {
    val toJavaCode = new ToJavaCode with SPPFMemoization
    toJavaCode.visit(node)
    toJavaCode.get
  }

}

class ToJavaCode extends Visitor[SPPFNode] with Id {

  override type T = Unit

  val sb = new StringBuilder

  def get = sb.toString

  def visit(node: SPPFNode): VisitResult[T] = node match {

    case n:NonterminalNode =>
      val children = n.children
      val name = n.name
      val slot = n.slot
      children.foreach(p => p.children.foreach(visit(_)))
      sb ++= s"""NonterminalNode node${getId(node)} = createNonterminalNode(registry.getSlot("$name"), registry.getSlot("${children.head.slot.toString}"), node${getId(children.head.leftChild)}, input);\n"""
      children.tail.foreach(c => sb ++= s"""node${getId(node)}.addPackedNode(registry.getSlot("${c.slot.toString}"), node${getId(c.leftChild)});\n""")
      None

    case IntermediateNode(name, leftExtent, rightExtent, children) =>
      children.foreach(p => p.children.foreach(visit(_)))
      sb ++= s"""IntermediateNode node${getId(node)} = createIntermediateNode(registry.getSlot("$name"), node${getId(children.head.leftChild)}, node${getId(children.head.rightChild)});\n"""
      children.tail.foreach(c => sb ++= s"""node${getId(node)}.addPackedNode(registry.getSlot("${c.slot.toString}"), node${getId(c.leftChild)}, node${getId(c.rightChild)});\n""")
      None

    case TerminalNode(name, leftExtent, rightExtent, input) =>
      sb ++= s"""TerminalNode node${getId(node)} = createTerminalNode(registry.getSlot("$name"), $leftExtent, $rightExtent, input);\n"""
      None
  }
}