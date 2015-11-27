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
    def call(s: Object, args: java.util.List[Object]): Object
    def nont(name: Object): Object
    def cond() = ???
    def code() = ???
    def term(name: Object): Object
  }
  
  class Pair[X,Y](val x: X, val y: Y)

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
                                 case "call" =>
                                 case "variable" =>
                                 case "labeled" =>
                                 case "constraint" =>
                                 case "binding" =>
                                 case "precede" =>
                                 case "notprecede" =>
                                 case "follow" =>
                                 case "notfollow" =>
                                 case "exclude" =>
                                 case "except" =>
                                 case "regex" =>
                                 case "nonterminal" =>
                                 case "string" =>
                                 case "character" =>
                               }
          case "symbols"    => return b.syms(build(children.head, b).asInstanceOf[java.util.List[Object]])
          case "parameters" =>
          case "arguments"  => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                               return skip(l.asInstanceOf[java.util.List[Object]], i => i==0||i%2==0)
          case "expression" =>
          case "return" =>
          case "binding" =>  
          
          case "regexbody"  =>  
          case "regexsequence" =>  
          case "regex" =>
          case "charclass" =>
          case "range" =>
          
          case "identifier" =>
          case "nontname" =>
          case "regexname" =>
          case "varname" =>
          case "label" =>
            
          case "attribute" =>
          case "associativity" =>
          case "altlabel" =>  
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