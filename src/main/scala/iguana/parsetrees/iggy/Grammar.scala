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
          case "alternate"  =>
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
    val l = new java.util.ArrayList[Object]()
    var i = 0
    children.foreach { child => if (i==0||i%2==0) l.add(build(child, b)); i = i + 1 } // Skip layout
    l
  }
  
  def skip(obj: Object): java.util.List[java.util.List[Object]] = {
    val list: Iterable[java.util.List[Object]] = obj.asInstanceOf[java.util.List[java.util.List[Object]]]
    val result = new java.util.ArrayList[java.util.List[Object]]()
    var i,j = 0
    list foreach {el =>
      if (i==0||i%2==0) {
        j = 0
        val l: Iterable[Object] = el
        val res = new java.util.ArrayList[Object]();
        result.add(res)
        l foreach { o => 
          if (j==0||j%2==0) res.add(o) }}
    }
    result
  }
  
  def rules(term: Tree) = term match { case Plus(trees) => }
  
}