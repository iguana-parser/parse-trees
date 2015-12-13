/*
 * Copyright (c) 2015, Ali Afroozeh and Anastasia Izmaylova, Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this 
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this 
 *    list of conditions and the following disclaimer in the documentation and/or 
 *    other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE.
 *
 */
package iguana.parsetrees.iggy

import java.util

import iguana.parsetrees.tree._
import scala.collection.JavaConversions._

/**
 * @author Anastasia Izmaylova
 */
trait Builder {
  def grammar(rules: java.util.List[Object]): Object
  def rule(tag: java.util.List[Object], name: Object, parameters: java.util.List[Object], body: java.util.List[Object]): Object
  def rule(name: Object, body: Object): Object
  def precGroup(ss: java.util.List[Object]): Object
  def assocGroup(ss: java.util.List[Object]): Object
  def body(ss: java.util.List[Object]): Object        // Sequence of symbols in the body of a single rule
  def star(s: Object): Object
  def plus(s: Object): Object
  def opt(s: Object): Object
  def seqGroup(s: java.util.List[Object]): Object
  def altGroup(s: java.util.List[Object]): Object
  def syms(ss: java.util.List[Object]): Object        // Represents a sequence of symbols inside an alternation group
  def regStar(s: Object): Object
  def regPlus(s: Object): Object
  def regOpt(s: Object): Object
  def regSeqGroup(ss: java.util.List[Object]): Object
  def regAltGroup(ss: java.util.List[Object]): Object
  def regs(ss: java.util.List[Object]): Object        // Represents a sequence of regex symbols inside an alternation group
  def nontCall(s: Object, args: java.util.List[Object]): Object
  def variable(name: Object, s: Object): Object
  def label(name: Object, s: Object): Object
  def restriction(s: Object, r: Object, kind: String): Object
  def constraints(cs: java.util.List[Object]): Object
  def bindings(ss: java.util.List[Object]): Object
  def assign(name: Object, exp: Object): Object
  def decl(name: Object, exp: Object): Object
  def funCall(s: Object, args: java.util.List[Object]): Object
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
  def lext(n: Object): Object
  def rext(n: Object): Object
  def yld(n: Object): Object
  def name(n: Object): Object
  def reg(name: Object): Object
  def number(n: Object): Object
  def nont(name: Object): Object
  def range(c: Object): Object
  def range(l: Object, r: Object): Object
  def charclass(rs: java.util.List[Object], kind: String): Object
  def string(s: Object): Object
}

object Grammar {
  
  def build(term: Tree, b: Builder): Object = {
    term match {
      case RuleNode(t, children: Seq[Tree], input) =>
        t.head.toLowerCase() match {
          case "definition" => return b.grammar(build(children.head, b).asInstanceOf[java.util.List[Object]])
          case "tag"        => return build(children.head, b)
          case "rule"       => t.label.toLowerCase() match {
                                 case "syntax" => val l = buildL(children, b, flatten = false);
                                                  return b.rule(l.get(0).asInstanceOf[java.util.List[Object]],
                                                                l.get(1),
                                                                l.get(2).asInstanceOf[java.util.List[Object]],
                                                                l.get(4).asInstanceOf[java.util.List[Object]])
                                 case "regex"  => val l = build(children.tail.tail.head, b).asInstanceOf[java.util.List[Object]]
                                                  val x = new util.ArrayList[Object]
                                                  var i = 0
                                                  0 until l.size() foreach {_ =>
                                                    if (i==0||i%3==0) x.add(b.rule(l.get(i), l.get(i + 2)))
                                                    i = i + 1
                                                  }
                                                  return x;
                                 case _        => throw new RuntimeException("Unknown type of rule: " + t.label)
                               }
          case "body"       => return skip(build(children.head, b).asInstanceOf[java.util.List[Object]], i => i==0||i%2==0)
          case "alternates" => return b.precGroup(skip(build(children.head, b).asInstanceOf[java.util.List[Object]], i => i==0||i%2==0))
          case "alternate"  => if (children.size == 1) return build(children.head, b)
                               else return b.assocGroup(skip(buildL(children, b), i => i!=0&&i%2!=0))
          case "sequence"   => return b.body(buildL(children, b))
          case "symbol"     => t.label.toLowerCase() match {
                                 case "star"        => return b.star(build(children.head, b))
                                 case "plus"        => return b.plus(build(children.head, b))
                                 case "option"      => return b.opt(build(children.head, b))
                                 case "sequence"    => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                                                       return b.seqGroup(l)
                                 case "alternation" => return b.altGroup(skip(buildL(children, b), i => i!=0&&i%2!=0))
                                 case "call"        => return b.nontCall(build(children.head, b), build(children.tail.head, b).asInstanceOf[java.util.List[Object]])
                                 case "variable"    => return b.variable(build(children.head, b), build(children.tail.tail.head, b))
                                 case "labeled"     => return b.variable(build(children.head, b), build(children.tail.tail.head, b))
                                 case "constraints" => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                                                       return b.constraints(skip(l.asInstanceOf[java.util.List[Object]], i => i==0||i%2==0))
                                 case "bindings"    => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                                                       return b.bindings(skip(l.asInstanceOf[java.util.List[Object]], i => i==0||i%2==0))
                                 case "precede"     => return b.restriction(build(children.tail.tail.head, b), build(children.head, b), "p")
                                 case "notprecede"  => return b.restriction(build(children.tail.tail.head, b), build(children.head, b), "np")
                                 case "follow"      => return b.restriction(build(children.head, b), build(children.tail.tail.head, b), "f")
                                 case "notfollow"   => return b.restriction(build(children.head, b), build(children.tail.tail.head, b), "nf")
                                 case "exclude"     => return b.restriction(build(children.head, b), build(children.tail.tail.head, b), "xcld")
                                 case "except"      => return b.restriction(build(children.head, b), build(children.tail.tail.head, b), "xpt")
                                 
                                 case "nonterminal" => return b.nont(build(children.head, b))
                                 
                                 case "regex"       => return build(children.tail.tail.head, b)
                                 case "string"      => return b.string(build(children.head, b))
                                 case "char"        => return b.string(build(children.head, b))
                                 case _             => throw new RuntimeException("Unknown type of symbol: " + t.label)
                               }
          case "symbols"    => return b.syms(build(children.head, b).asInstanceOf[java.util.List[Object]])
          case "parameters" => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                               return skip(l.asInstanceOf[java.util.List[Object]], i => i==0||i%2==0)
          case "arguments"  => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                               return skip(l.asInstanceOf[java.util.List[Object]], i => i==0||i%2==0)
          case "expression" => t.label.toLowerCase() match {
                                 case "call"           => return b.funCall(build(children.head, b), build(children.tail.head, b).asInstanceOf[java.util.List[Object]])
                                 case "multiplication" => return b.mult(build(children.head, b), build(children.tail.tail.head, b))
                                 case "division"       => return b.div(build(children.head, b), build(children.tail.tail.head, b))
                                 case "plus"           => return b.plus(build(children.head, b), build(children.tail.tail.head, b))
                                 case "minus"          => return b.minus(build(children.head, b), build(children.tail.tail.head, b))
                                 case "greatereq"      => return b.greatereq(build(children.head, b), build(children.tail.tail.head, b))
                                 case "lesseq"         => return b.lesseq(build(children.head, b), build(children.tail.tail.head, b))
                                 case "greater"        => return b.greater(build(children.head, b), build(children.tail.tail.head, b))
                                 case "less"           => return b.less(build(children.head, b), build(children.tail.tail.head, b))
                                 case "equal"          => return b.equal(build(children.head, b), build(children.tail.tail.head, b))
                                 case "notequal"       => return b.notequal(build(children.head, b), build(children.tail.tail.head, b))
                                 case "and"            => return b.and(build(children.head, b), build(children.tail.tail.head, b))
                                 case "or"             => return b.or(build(children.head, b), build(children.tail.tail.head, b))
                                 case "lextent"        => return b.lext(build(children.head, b))
                                 case "rextent"        => return b.rext(build(children.head, b))
                                 case "yield"          => return b.yld(build(children.head, b))
                                 case "name"           => return b.name(build(children.head, b))
                                 case "number"         => return b.name(build(children.head, b))
                                 case "bracket"        => return build(children.tail.head, b)
                                 case _                => throw new RuntimeException("Unknown type of expression: " + t.label)
                               }
          case "return"     => return build(children.tail.head, b)
          case "binding"    => t.label.toLowerCase() match {
                                 case "assignment"  => return b.assign(build(children.head, b), build(children.tail.tail.head, b))
                                 case "declaration" => return b.decl(build(children.tail.head, b), build(children.tail.tail.tail.head, b))
                                 case _             => throw new RuntimeException("Unknown type of binding: " + t.label)
                               }          
          case "regexbody"     => return b.regAltGroup(skip(build(children.head, b).asInstanceOf[java.util.List[Object]], i => i==0||i%2==0))
          case "regexsequence" => return b.regSeqGroup(build(children.head, b).asInstanceOf[java.util.List[Object]])
          case "regex"         => t.label.toLowerCase() match {
                                    case "star"        => return b.regStar(build(children.head, b))
                                    case "plus"        => return b.regPlus(build(children.head, b))
                                    case "option"      => return b.regOpt(build(children.head, b))
                                    case "sequence"    => val l = buildL(children, b); l.remove(0); l.remove(l.size() - 1)
                                                          return b.regSeqGroup(l)
                                    case "alternation" => return b.regAltGroup(skip(buildL(children, b), i => i!=0&&i%2!=0))
                                    case "bracket"     => return build(children.tail.head, b)
                                    case "nonterminal" => return b.reg(build(children.head, b))
                                    case "charclass"   => return build(children.head, b)
                                    case "string"      => return b.string(build(children.head, b))
                                    case "char"        => return b.string(build(children.head, b))
                                    case _             => throw new RuntimeException("Unknown type of regex: " + t.label)
                                  }
          case "regexs"        => return b.regs(build(children.head, b).asInstanceOf[java.util.List[Object]])
          case "charclass"     => t.label.toLowerCase() match {
                                    case "thesechars"    => return b.charclass(build(children.tail.head, b).asInstanceOf[java.util.List[Object]], "")
                                    case "notthesechars" => return b.charclass(build(children.tail.head, b).asInstanceOf[java.util.List[Object]], "not")
                                  }
          case "range"         => if (children.size == 1) return b.range(build(children.head, b))
                                  else return b.range(build(children.head, b), build(children.tail.head, b))

          case "identifier" => return build(children.head, b)
          case "nontname"   => return build(children.head, b)
          case "regexname"  => return build(children.head, b)
          case "varname"    => return build(children.head, b)
          case "label"      => return build(children.head, b)
            
          case "attribute"     => return build(children.head, b)
          case "associativity" => return build(children.head, b)
          case "altlabel"      => return build(children.tail.head, b)
        }
        
      case Plus(children)  => return buildL(children, b)
      case Star(children)  => return buildL(children, b)
      case Opt(child)      => val l = new java.util.ArrayList[Object]
                              val x = build(child, b)
                              if (x.isInstanceOf[java.util.List[_]])
                                l.addAll(x.asInstanceOf[java.util.List[Object]])
                              else if (!x.equals(None))
                                l.add(x)
                              return l
      case Group(children) => return buildL(children, b)
      
      case Terminal(name, i, j, input) => return input.subString(i, j)
      case Epsilon(_) => return None
        
      case _ => 
        throw new RuntimeException("Unknown node: " + term);
    }
    
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