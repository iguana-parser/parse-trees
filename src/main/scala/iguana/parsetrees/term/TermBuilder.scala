package iguana.parsetrees.term

import iguana.parsetrees.slot.TerminalNodeType.TerminalNodeType
import iguana.utils.input.Input
import iguana.parsetrees.slot.Action

/**
  * TODO: This naming is very confusing. Try to unify them with the grammar model when the grammar model is ready.
  */
trait RuleType {
  def head: String
  def label: String
  def body: java.util.List[String]
  def action: Action
  def position: Int
  def layout: Boolean
}

trait TerminalType {
  def name: String
  def nodeType: TerminalNodeType
}

/**
  * The [[TermBuilder]] trait provides the methods necessary for building terms.
  * The default implementation of [[TermBuilder]], [[DefaultTermBuilder]] creates
  * our standard terms. However, users can provide their own impelementation of
  * terms.
  *
  * @tparam T
  */
trait TermBuilder[T] {
  def terminalTerm(terminalType: TerminalType, l: Int, r: Int, input: Input): T
  def nonterminalTerm(ruleType: RuleType, children: Seq[T], l: Int, r: Int, input: Input): T
  def ambiguityTerm(children: Seq[Seq[T]]): T
  def star(children: Seq[T]): T
  def plus(children: Seq[T]): T
  def alt(l: Seq[T]): T
  def opt(child: T): T
  def group(children: Seq[T]): T
  def cycle(label: String): T
  def epsilon(i: Int): T
}

class DefaultTermBuilder(input: Input) extends TermBuilder[Term] {

  override def terminalTerm(terminalType: TerminalType, l: Int, r: Int, input: Input): Term = TerminalTerm(terminalType, l, r, input)

  override def nonterminalTerm(ruleType: RuleType, children: Seq[Term], l: Int, r: Int, input: Input): Term =
    NonterminalTerm(ruleType, children, input)

  override def ambiguityTerm(children: Seq[Seq[Term]]): Term = AmbiguityTerm(children)

  override def cycle(label: String) = Cycle(label)

  override def epsilon(i: Int): Term = Epsilon(i)

  override def star(children: Seq[Term]): Term = Star(children)

  override def plus(children: Seq[Term]): Term = Plus(children)

  override def group(children: Seq[Term]): Term = Group(children)

  override def alt(children: Seq[Term]): Term = Alt(children)

  override def opt(child: Term): Term = Opt(child)

}
