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
    def star(s: Object): Object
    def plus(s: Object): Object
    def opt(s: Object): Object
    def seq(s: java.util.List[Object]): Object 
    def alt(s: java.util.List[Object]): Object
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
          case "rule"       => val l = build(children, b);
                               
                               if (l.size() == 2) return b.rule(l.get(0), l.get(2))
                               else return b.rule(l.get(0), l.get(1), l.get(2), l.get(4))
          case "body"       => skip(build(children.head, b))
          case "alternate"  => val x = new java.util.ArrayList[Object]
                               val y = new java.util.ArrayList[String]
                               if (children.size == 1) 
                                 x.add(build(children.head, b)) 
                               else {
                                 val l = build(children, b)
                                 val snd = skip(l.get(2).asInstanceOf[java.util.List[Object]])
                                 x.add(l.get(1))
                                 x.addAll(snd)
                                 y.add(l.get(4).asInstanceOf[String])
                               }
                               return new Pair(x,y)
          case "sequence"   =>
          
          
          case "symbol" =>
          case "parameters" =>
          case "arguments" =>
          
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
        }
        
      case Plus(children) => return build(children, b)
      case Star(children) => return build(children, b)
      case Opt(child) => val l = new java.util.ArrayList[Object](); l.add(build(child, b)); return l
      
      case Terminal(name, _, _, _) => return name
      case Epsilon(_) => return ""     
        
      case _ => 
        throw new RuntimeException("Unknown node: " + term);
    }
    
    ???
  }
  
  def build(children: Seq[Tree], b: Builder): java.util.List[Object] = {
    val l = new java.util.ArrayList[Object]
    var i = 0
    children.foreach { child => if (i==0||i%2==0) l.add(build(child, b)); i = i + 1 } // Skip layout
    l
  }
  
  def skip(obj: Object): java.util.List[java.util.List[Object]] = {
    val result = new java.util.ArrayList[java.util.List[Object]]
    var i = 0
    asScalaBuffer(obj.asInstanceOf[java.util.List[Object]]) foreach { el =>
      if (i==0||i%2==0) result.add(skip(el.asInstanceOf[java.util.List[Object]])) // Skip delimiters
    }
    result
  }
  
  def skip(obj: java.util.List[Object]): java.util.List[Object] = {
    val l = new java.util.ArrayList[Object]
    var i = 0
    asScalaBuffer(obj) foreach { o => if (i==0||i%2==0) l.add(o)} // Skip delimiters
    l
  }
  
  def flatten() = {
    ???
  }
  
}