package iguana.parsetrees.term

import iguana.parsetrees.slot.TerminalNodeType
import iguana.parsetrees.sppf.NonterminalNode
import iguana.utils.input.Input
import Utils._

import scala.collection.JavaConverters._

trait Term extends Product {
  def leftExtent: Int
  def rightExtent: Int

  /**
    * For visualization purpose it is desirable keep the default equality intact, i.e.,
    * using object references.
    */
  def equals(t: Term): Boolean
}

class NonterminalTerm(val r: RuleType,
                      val ts: Seq[Term],
                      val input: Input) extends Term{
  override def leftExtent= ts.head.leftExtent
  override def rightExtent = ts.last.rightExtent
  def isLayout: Boolean = r.layout

  override def equals(t: Term): Boolean =  t match {
    case n: NonterminalTerm => leftExtent == n.leftExtent &&
      rightExtent == n.rightExtent &&
      r.head  == n.r.head &&
      ts.zip(n.ts).forall { case (t1, t2) => t1 equals t2 }
    case _           => false
  }

  override def productElement(n: Int): Any = n match {
    case 0 => r
    case 1 => ts
    case 2 => input
    case _ => throw new IndexOutOfBoundsException(n.toString)
  }

  override def productArity: Int = 3

  override def canEqual(that: Any): Boolean = this.isInstanceOf[NonterminalTerm]
}

object NonterminalTerm {
  def apply(r: RuleType, ts: Seq[Term], input: Input) = new NonterminalTerm(r, ts, input)
  def unapply(r: NonterminalTerm): Option[(RuleType, Seq[Term], Input)] = Some(r.r, r.ts, r.input)
}

object NonterminalTermNoLayout {
  def unapply(nt: NonterminalTerm): Option[(String, Seq[Term])] = Some(nt.r.head, noLayout(nt.ts))
}

object SeqNoLayout {
  def unapplySeq(l: Seq[Term]): Option[Seq[Term]] = Some(noLayout(l))
}

object NonterminalTermName {
  def unapply(nt: NonterminalTerm): Option[String] = Some(nt.r.head)
}

class AmbiguityTerm(val ts: Seq[AmbiguityBranch[Term]]) extends Term with TermListProduct {
  override def leftExtent: Int = ts.head.leftExtent
  override def rightExtent: Int = ts.last.rightExtent

  override def equals(t: Term): Boolean = t match {
    case a: AmbiguityTerm =>
      leftExtent  == a.leftExtent &&
      rightExtent == a.rightExtent &&
      ts.zip(a.ts).forall { case (b1, b2) => b1.children.zip(b2.children).forall { case (t1, t2) => t1 equals t2 } }
    case _ => false
  }
}

object AmbiguityTerm {
  def apply(ts: Seq[AmbiguityBranch[Term]]) = new AmbiguityTerm(ts)
  def unapply(a: AmbiguityTerm): Option[Seq[AmbiguityBranch[Term]]] = Some(a.ts)
}

case class NonterminalAmbiguityBranch(val rt: RuleType, val children: Seq[Term], val input: Input) extends AmbiguityBranch[Term] {
  override def leftExtent: Int = children.head.leftExtent
  override def rightExtent: Int = children.last.rightExtent
}

case class IntermediateAmbiguityBranch(val children: Seq[Term]) extends AmbiguityBranch[Term] {
  override def leftExtent: Int = children.head.leftExtent
  override def rightExtent: Int = children.last.rightExtent
}

class TerminalTerm(val tt: TerminalType,
                   val leftExtent: Int,
                   val rightExtent: Int,
                   val input: Input) extends Term with Product {

  def isLayout: Boolean = tt.nodeType == TerminalNodeType.Layout

  override def equals(t: Term): Boolean = t match {
    case t:TerminalTerm => leftExtent == t.leftExtent &&
      rightExtent == t.rightExtent &&
      tt == t.tt
    case _          => false
  }

  override def productElement(n: Int): Any = n match {
    case 0 => tt
    case 1 => leftExtent
    case 2 => rightExtent
    case 3 => input
    case _ => throw new IndexOutOfBoundsException(n.toString)
  }
  override def productArity: Int = 4
  override def canEqual(that: Any): Boolean = this.isInstanceOf[TerminalTerm]
}

object TerminalTerm {
  def apply(tt: TerminalType, leftExtent: Int, rightExtent: Int, input: Input) =
    new TerminalTerm(tt, leftExtent, rightExtent, input)

  def unapply(t: TerminalTerm): Option[(TerminalType, Int, Int, Input)] = Some(t.tt, t.leftExtent, t.rightExtent, t.input)
}

object TerminalTermName {
  def unapply(tt: TerminalTerm): Option[String] = Some(tt.tt.name)
}

class Epsilon(val i: Int) extends Term with Product {
  override def leftExtent = i
  override def rightExtent = i

  override def equals(t: Term): Boolean = t match {
    case e: Epsilon => i == e.i
    case _          => false
  }

  override def productElement(n: Int): Any = i
  override def productArity: Int = 1
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Epsilon]
}

object Epsilon {
  def apply(i: Int) = new Epsilon(i)
  def unapply(e: Epsilon): Option[Int] = Some(e.i)
}

case class Cycle(node: NonterminalNode) extends Term {
  override def leftExtent = -1
  override def rightExtent = -1

  override def equals(t: Term): Boolean = t match {
    case c:Cycle => node deepEquals c.node
    case _       => false
  }
}

class Star(val ts: Seq[Term]) extends Term with TermListProduct {
  override def leftExtent = ts.head.leftExtent
  override def rightExtent = ts.last.rightExtent

  override def equals(t: Term): Boolean = t match {
    case s:Star => ts.zip(s.ts).forall { case (t1, t2) => t1 equals t2 }
    case _      => false
  }
}

object Star {
  def apply(children: Seq[Term]) = new Star(children)
  def unapply(s: Star): Option[Seq[Term]] = Some(s.ts)
}

class Plus(val ts: Seq[Term]) extends Term with TermListProduct {
  override def leftExtent = ts.head.leftExtent
  override def rightExtent = ts.last.rightExtent

  override def equals(t: Term): Boolean = t match {
    case p:Plus => ts.zip(p.ts).forall { case (t1, t2) => t1 equals t2 }
    case _      => false
  }
}

object Plus {
  def apply(children: Seq[Term]) = new Plus(children)
  def unapply(p: Plus): Option[Seq[Term]] = Some(p.ts)
}

class Group(val ts: Seq[Term]) extends Term with TermListProduct {
  override def leftExtent = ts.head.leftExtent
  override def rightExtent = ts.last.rightExtent

  override def equals(t: Term): Boolean = t match {
    case g:Group => ts.zip(g.ts).forall { case (t1, t2) => t1 equals t2 }
    case _      => false
  }
}

object Group {
  def apply(ts: Seq[Term]) = new Group(ts)
  def unapply(g: Group): Option[Seq[Term]] = Some(g.ts)
}

class Opt(val child: Term) extends Term with TermProduct {
  override def leftExtent = child.leftExtent
  override def rightExtent = child.rightExtent

  override def equals(t: Term): Boolean = t match {
    case o:Opt => child.equals(o.child)
    case _      => false
  }
}

object Opt {
  def apply(child: Term) = new Opt(child)
  def unapply(o: Opt): Option[Term] = Some(o.child)
}

class Alt(val ts: Seq[Term]) extends Term with TermListProduct {
  override def leftExtent = ts.head.leftExtent
  override def rightExtent = ts.last.rightExtent

  override def equals(t: Term): Boolean = t match {
    case a:Alt => ts.zip(a.ts).forall { case (t1, t2) => t1 equals t2 }
    case _      => false
  }
}

object Alt {
  def apply(children: Seq[Term]) = new Alt(children)
  def unapply(a: Alt): Option[Seq[Term]] = Some(a.ts)
}

/**
  * Factory object for creating terms from Java
  */
object TermFactory {
  def createNonterminalTerm(ruleType: RuleType, children: java.util.List[Term], input: Input) = NonterminalTerm(ruleType, children.asScala, input)
  def createAmbiguityTerm(children: java.util.List[AmbiguityBranch[Term]]) = AmbiguityTerm(children.asScala)
  def createNonterminalAmbiguityBranch(ruleType: RuleType, children: java.util.List[Term], input: Input) = NonterminalAmbiguityBranch(ruleType, children.asScala, input)
  def createIntermediateAmbiguityBranch(children: java.util.List[Term]) = IntermediateAmbiguityBranch(children.asScala)
  def createTerminalTerm(terminalType: TerminalType, leftExtent: Int, rightExtent:Int, input: Input) = TerminalTerm(terminalType, leftExtent, rightExtent, input)
  def createEpsilon(i: Int) = Epsilon(i)
  def createCycle(a: NonterminalNode) = Cycle(a)
  def createStar(children: java.util.List[Term]) = Star(children.asScala)
  def createPlus(children: java.util.List[Term]) = Plus(children.asScala)
  def createGroup(children: java.util.List[Term]) = Group(children.asScala)
  def createAlt(children: java.util.List[Term]) = Alt(children.asScala)
  def createOpt(child: Term) = Opt(child)
}

object Utils {

  trait TermListProduct extends Product1[Seq[Any]] {
    def ts: Seq[Any]
    override def _1: Seq[Any] = ts
    override def canEqual(that: Any): Boolean = that.isInstanceOf[TermListProduct]
  }

  trait TermProduct extends Product1[Term] {
    def child: Term
    override def _1: Term = child
    override def canEqual(that: Any): Boolean = that.isInstanceOf[TermProduct]
  }

  def noLayout(l: Seq[Term]): Seq[Term] = l.filter {
    case n: NonterminalTerm => !n.isLayout
    case t: TerminalTerm => !t.isLayout
  }
}
