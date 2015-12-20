
package iguana.parsetrees.slot

object NonterminalNodeType {

  type NonterminalNodeType = Int

  val Basic  = 0
  val Layout = 1
  val Star   = 2
  val Plus   = 3
  val Opt    = 4
  val Seq    = 5
  val Alt    = 6
}

object TerminalNodeType {

  type TerminalNodeType = Int

  val Regular = 0
  val Layout = 1
}

import NonterminalNodeType._
import TerminalNodeType._
import iguana.parsetrees.term.RuleType
import iguana.parsetrees.term.TerminalType

trait Slot

trait PackedNodeSlot extends Slot {
  def ruleType: RuleType
}

trait TerminalSlot extends Slot {
  def terminalType: TerminalType
}

trait NonterminalSlot extends Slot {
  def nodeType: NonterminalNodeType
}

trait Action {
  def apply(a: Any): Any
}

case object DummySlot extends Slot {
  override def toString = "$"
}
