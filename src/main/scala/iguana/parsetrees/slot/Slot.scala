
package iguana.parsetrees.slot

object NonterminalNodeType {

  type NonterminalNodeType = Int

  val Basic = 0
  val Star  = 1
  val Plus  = 2
  val Opt   = 3
  val Seq   = 4
  val Alt   = 5
}

import NonterminalNodeType._
import iguana.parsetrees.tree.RuleType

trait Slot

trait PackedNodeSlot extends Slot {
  def ruleType: RuleType
}

trait TerminalSlot extends Slot {
  def terminalName: String
}

trait NonterminalSlot extends Slot {
  def nodeType: NonterminalNodeType
}

trait Action {
  def apply(a: Any): Any
}

case object DummySlot extends Slot
