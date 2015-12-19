package iguana.parsetrees.term

import iguana.parsetrees.slot.TerminalNodeType
import iguana.utils.input.Input

import scala.collection.JavaConverters._

trait Term {
  def leftExtent: Int
  def rightExtent: Int

  def equals(t: Term): Boolean
}

object TermFactory {
  def createRule(ruleType: RuleType, children: java.util.List[Term], input: Input) = NonterminalTerm(ruleType, children.asScala, input)
  def createAmbiguity(children: java.util.List[java.util.List[Term]]) = AmbiguityTerm(asScala(children))
  def createTerminal(terminalType: TerminalType, leftExtent: Int, rightExtent:Int, input: Input) = TerminalTerm(terminalType, leftExtent, rightExtent, input)
  def createEpsilon(i: Int) = Epsilon(i)
  def createCycle(label: String) = Cycle(label)
  def createStar(children: java.util.List[Term]) = Star(children.asScala)
  def createPlus(children: java.util.List[Term]) = Plus(children.asScala)
  def createGroup(children: java.util.List[Term]) = Group(children.asScala)
  def createAlt(children: java.util.List[Term]) = Alt(children.asScala)
  def createOpt(child: Term) = Opt(child)

  def asScala[T](list: java.util.List[java.util.List[T]]): Seq[Seq[T]] = list.asScala.map(x => x.asScala)
}

class NonterminalTerm(val r: RuleType, val ts: Seq[Term], val input: Input) extends Term {
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
}

object NonterminalTerm {
  def apply(r: RuleType, ts: Seq[Term], input: Input) = new NonterminalTerm(r, ts, input)
  def unapply(r: NonterminalTerm): Option[(RuleType, Seq[Term], Input)] = Some(r.r, r.ts, r.input)
}

object NT {
  def unapply(nt: NonterminalTerm): Option[(String, Seq[Term])] =
    Some(nt.r.head, nt.ts.filter {
                                case n: NonterminalTerm => !n.isLayout
                                case t: TerminalTerm    => !t.isLayout
                               })
}

object NTL {
  def unapply(nt: NonterminalTerm): Option[(String, Seq[Term])] = Some(nt.r.head, nt.ts)
}


class AmbiguityTerm(val ts: Seq[Seq[Term]]) extends Term {
  override def leftExtent: Int = ts.head.head.leftExtent
  override def rightExtent: Int = ts.head.last.rightExtent

  override def equals(t: Term): Boolean = t match {
    case a: AmbiguityTerm => leftExtent == a.leftExtent &&
                   rightExtent == a.rightExtent &&
                   ts.zip(a.ts).forall { case (t1, t2) => t1.zip(t2).forall { case (z1, z2) => z1 equals z2 } }
    case _      => false
  }
}

object AmbiguityTerm {
  def apply(ts: Seq[Seq[Term]]) = new AmbiguityTerm(ts)
  def unapply(a: AmbiguityTerm): Option[Seq[Seq[Term]]] = Some(a.ts)
}

class TerminalTerm(val tt: TerminalType, val leftExtent: Int, val rightExtent: Int, val input: Input) extends Term {
  def isLayout: Boolean = tt.nodeType == TerminalNodeType.Layout

  override def equals(t: Term): Boolean = t match {
    case t:TerminalTerm => leftExtent == t.leftExtent &&
                       rightExtent == t.rightExtent &&
                       tt == t.tt
    case _          => false
  }
}

object TerminalTerm {

  def apply(tt: TerminalType, leftExtent: Int, rightExtent: Int, input: Input) =
    new TerminalTerm(tt, leftExtent, rightExtent, input)

  def unapply(t: TerminalTerm): Option[(TerminalType, Int, Int, Input)] = Some(t.tt, t.leftExtent, t.rightExtent, t.input)
}

object TT {
  def unapply(tt: TerminalTerm): Option[String] = Some(tt.tt.name)
}

class Epsilon(val i: Int) extends Term {
  override def leftExtent = i
  override def rightExtent = i

  override def equals(t: Term): Boolean = t match {
    case e: Epsilon => i == e.i
    case _          => false
  }
}

object Epsilon {
  def apply(i: Int) = new Epsilon(i)
  def unapply(e: Epsilon): Option[Int] = Some(e.i)
}

class Cycle(val label: String) extends Term {
  override def leftExtent = -1
  override def rightExtent = -1

  override def equals(t: Term): Boolean = t match {
    case c:Cycle => label == c.label
    case _       => false
  }
}

object Cycle {
  def apply(label: String) = new Cycle(label)
  def unapply(c: Cycle): Option[String] = Some(c.label)
}

class Star(val children: Seq[Term]) extends Term {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent

  override def equals(t: Term): Boolean = t match {
    case s:Star => children.zip(s.children).forall { case (t1, t2) => t1 equals t2 }
    case _      => false
  }
}

object Star {
  def apply(children: Seq[Term]) = new Star(children)
  def unapply(s: Star): Option[Seq[Term]] = Some(s.children)
}

class Plus(val children: Seq[Term]) extends Term {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent

  override def equals(t: Term): Boolean = t match {
    case p:Plus => children.zip(p.children).forall { case (t1, t2) => t1 equals t2 }
    case _      => false
  }
}

object Plus {
  def apply(children: Seq[Term]) = new Plus(children)
  def unapply(p: Plus): Option[Seq[Term]] = Some(p.children)
}

class Group(val children: Seq[Term]) extends Term {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent

  override def equals(t: Term): Boolean = t match {
    case g:Group => children.zip(g.children).forall { case (t1, t2) => t1 equals t2 }
    case _      => false
  }
}

object Group {
  def apply(children: Seq[Term]) = new Group(children)
  def unapply(g: Group): Option[Seq[Term]] = Some(g.children)
}

class Opt(val child: Term) extends Term {
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

class Alt(val children: Seq[Term]) extends Term {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent

  override def equals(t: Term): Boolean = t match {
    case a:Alt => children.zip(a.children).forall { case (t1, t2) => t1 equals t2 }
    case _      => false
  }
}

object Alt {
  def apply(children: Seq[Term]) = new Alt(children)
  def unapply(a: Alt): Option[Seq[Term]] = Some(a.children)
}

