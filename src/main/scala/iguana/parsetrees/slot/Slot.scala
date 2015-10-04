
package iguana.parsetrees.slot

import iguana.parsetrees.sppf.NonterminalNodeType.NonterminalNodeType

trait Slot

trait NonterminalSlot extends Slot {
  def ruleType: Any
  def nodeType: NonterminalNodeType
}

trait TerminalSlot extends Slot {
  def terminalName: String
}
