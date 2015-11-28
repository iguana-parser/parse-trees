package iguana.parsetrees.iggy

import iguana.parsetrees.tree._
import scala.collection.JavaConversions._

/**
 * @author Anastasia Izmaylova
 */
trait Grammar {
  
  trait Builder {
    def grammar(rules: java.util.List[Object]): Object
    def rule(tag: Object, name: Object, parameters: Object, body: Object): Object
    def rule(name: Object, body: Object): Object
    def precGs(ss: java.util.List[Object]): Object
    def precG(ss: java.util.List[Object]): Object
    def assocG(ss: java.util.List[Object]): Object
    def body(ss: java.util.List[Object]): Object
    def syms(ss: java.util.List[Object]): Object
    def star(s: Object): Object
    def plus(s: Object): Object
    def opt(s: Object): Object
    def seqG(s: java.util.List[Object]): Object 
    def altG(s: java.util.List[Object]): Object
    def regStar(s: Object): Object
    def regPlus(s: Object): Object
    def regOpt(s: Object): Object
    def regSeqG(s: java.util.List[Object]): Object 
    def regAltG(s: java.util.List[Object]): Object
    def callS(s: Object, args: java.util.List[Object]): Object
    def variable(name: Object, s: Object): Object
    def label(name: Object, s: Object): Object
    def restriction(s: Object, r: Object, kind: String): Object
    def constraints(cs: java.util.List[Object]): Object
    def bindings(ss: java.util.List[Object]): Object
    def assign(name: Object, exp: Object): Object
    def decl(name: Object, exp: Object): Object
    def callE(s: Object, args: java.util.List[Object]): Object
    def mult(l: Object, r: Object): Object
    def div(l: Object, r: Object): Object
    def plus(l: Object, r: Object): Object
    def minus(l: Object, r: Object): Object
    def greatereq(l: Object, r: Object): Object
    def lesseq(l: Object, r: Object): Object
    def greater(l: Object, r: Object): Object
    def less(l: Object, r: Object): Object
    def equal(l: Object, r: Object): Object
    def notequal(l: Object, r: Object): Object
    def and(l: Object, r: Object): Object
    def or(l: Object, r: Object): Object
    def name(n: Object): Object
    def number(n: Object): Object
    def nont(name: Object): Object
    def term(name: Object): Object
    def range(l: Object, r: Object): Object
    def charclass(rs: java.util.List[Object]): Object
  }
  
  def build(term: Tree, b: Builder): Object = {
    term match {
      case RuleNode(t, children: Seq[Tree], _) =>       
        t.head.toLowerCase() match {
          case "definition" => b.grammar(build(children.head, b).asInstanceOf[java.util.List[Object]])
          case "tag"        => build(children.head, b)
          case "rule"       => val l = buildL(children, b, flatten = false);
                               if (l.size() == 2) return b.rule(l.get(0), l.get(2))
                               else return b.rule(l.get(0), l.get(1), l.get(2), l.get(4))
          case "body"       => return b.precGs(skip(build(children.head, b).asInstanceOf[java.util.List[Object]], i => i==0||i%2==0))
          case "alternates" => return b.precG(skip(build(children.head, b).asInstanceOf[java.util.List[Object]], i => i==0||i%2==0))
          case "alternate"  => if (children.size == 1) return build(children.head, b)
                               else return b.assocG(skip(buildL(children, b), i => i!=0&&i%2!=0))
          case "sequence"   => return b.body(buildL(children, b))
          case "symbol"     => t.label.toLowerCase() match {
                                 case "star"        => return b.star(build(children.head, b))
                                 case "plus"        => return b.plus(build(children.head, b))
                                 case "option"      => return b.opt(build(children.head, b))
                                 case "sequence"    => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                                                       return b.seqG(l)
                                 case "alternation" => return b.altG(skip(buildL(children, b), i => i!=0&&i%2!=0))
                                 case "call"        => return b.callS(build(children.head, b), build(children.tail.head, b).asInstanceOf[java.util.List[Object]]) 
                                 case "variable"    => return b.variable(build(children.head, b), build(children.tail.tail.head, b))
                                 case "labeled"     => return b.variable(build(children.head, b), build(children.tail.tail.head, b))
                                 case "constraint"  => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                                                       return b.constraints(skip(l.asInstanceOf[java.util.List[Object]], i => i==0||i%2==0))
                                 case "binding"     => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                                                       return b.bindings(skip(l.asInstanceOf[java.util.List[Object]], i => i==0||i%2==0))
                                 case "precede"     => return b.restriction(build(children.tail.tail.head, b), build(children.head, b), "p")
                                 case "notprecede"  => return b.restriction(build(children.tail.tail.head, b), build(children.head, b), "np")
                                 case "follow"      => return b.restriction(build(children.head, b), build(children.tail.tail.head, b), "f")
                                 case "notfollow"   => return b.restriction(build(children.head, b), build(children.tail.tail.head, b), "nf")
                                 case "exclude"     => return b.restriction(build(children.head, b), build(children.tail.tail.head, b), "xcld")
                                 case "except"      => return b.restriction(build(children.head, b), build(children.tail.tail.head, b), "xpt")
                                 
                                 case "nonterminal" => return b.nont(build(children.head, b))
                                 
                                 case "regex"       => 
                                 case "string"      => return b.term(build(children.head, b))
                                 case "character"   => return b.term(build(children.head, b))
                               }
          case "symbols"    => return b.syms(build(children.head, b).asInstanceOf[java.util.List[Object]])
          case "parameters" => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                               return skip(l.asInstanceOf[java.util.List[Object]], i => i==0||i%2==0)
          case "arguments"  => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                               return skip(l.asInstanceOf[java.util.List[Object]], i => i==0||i%2==0)
          case "expression" => t.label.toLowerCase() match {
                                 case "call"           => 
                                 case "multiplication" =>
                                 case "division"       =>
                                 case "plus"           =>
                                 case "minus"          =>
                                 case "greatereq"      =>
                                 case "lesseq"         =>
                                 case "greater"        =>
                                 case "less"           =>
                                   
                               }
          case "return"     => return build(children.tail.head, b)
          case "binding"    => t.label.toLowerCase() match {
                                 case "assignment"  => return b.assign(build(children.head, b), build(children.tail.tail.head, b))
                                 case "declaration" => return b.decl(build(children.tail.head, b), build(children.tail.tail.tail.head, b))
                               }
          
          case "regexbody"     =>  
          case "regexsequence" =>  
          case "regex"         => t.label.toLowerCase() match {
                                    case "star"        => return b.regStar(build(children.head, b))
                                    case "plus"        => return b.regPlus(build(children.head, b))
                                    case "option"      => return b.regOpt(build(children.head, b))
                                    case "sequence"    => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                                                          return b.regSeqG(l)
                                    case "alternation" => return b.regAltG(skip(buildL(children, b), i => i!=0&&i%2!=0))
                                    case "bracket"     => return build(children.tail.head, b)
                                    case "nonterminal" => return build(children.head, b)
                                    case "charclass"   => 
                                    case "string"      => return b.term(build(children.head, b))
                                    case "char"        => return b.term(build(children.head, b))
                                  }
          case "charclass"     =>
          case "range"         =>
          
          case "identifier" => return build(children.head, b)
          case "nontname"   => return build(children.head, b)
          case "regexname"  => return build(children.head, b)
          case "varname"    => return build(children.head, b)
          case "label"      => return build(children.head, b)
            
          case "attribute"  => return build(children.head, b)
          case "associativity" => return build(children.head, b)
          case "altlabel"   => 
        }
        
      case Plus(children)  => return buildL(children, b)
      case Star(children)  => return buildL(children, b)
      case Opt(child)      => val l = new java.util.ArrayList[Object]
                              val x = build(child, b)
                              if (x.isInstanceOf[java.util.List[_]])
                                l.addAll(x.asInstanceOf[java.util.List[Object]])
                              else
                                l.add(x)
                              return l
      case Group(children) => return buildL(children, b)
      
      case Terminal(name, _, _, _) => return name
      case Epsilon(_) => return ""
        
      case _ => 
        throw new RuntimeException("Unknown node: " + term);
    }
    
    ???
  }
  
  def buildL(children: Seq[Tree], b: Builder, flatten: Boolean = true): java.util.List[Object] = {
    val l = new java.util.ArrayList[Object]
    var i = 0
    children.foreach { child => // Skip layout 
      if (i==0||i%2==0) {
        val x = build(child, b)
        if (flatten && x.isInstanceOf[java.util.List[_]])
          l.addAll(x.asInstanceOf[java.util.List[Object]])
        else
          l.add(x)
      }
      i = i + 1 
    } 
    l
  }
  
  def skip(obj: java.util.List[Object], p: Int => Boolean): java.util.List[Object] = {
    if (obj.size() <= 1) return obj
    val l = new java.util.ArrayList[Object]
    var i = 0
    asScalaBuffer(obj) foreach { o => if (p(i)) l.add(o); i = i + 1 } // Skip delimiters
    l
  }
  
}