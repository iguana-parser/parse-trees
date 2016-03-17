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

import iguana.parsetrees.term._
import scala.collection.JavaConversions._
import java.lang.reflect.{InvocationTargetException, Method}

/**
 * @author Anastasia Izmaylova
 */
object TermTraversal {
  
  trait Actions
  
  def build(term: Term, b: Actions): Object = {
    term match {
      case NonterminalTerm(t, ns, _) if (t.label == null || t.label.isEmpty())
        =>
           if (ns.size == 1) return build(ns.head, b);
           val x = skip(buildL(ns, b))
           if (x.size == 1) return x(0)
           return x

      case NonterminalTerm(t, ns, _) if (t.label != null && !t.label.isEmpty)
        =>
           if (t.label.toLowerCase.equals("start"))
             return build(ns.tail.head, b)
           else {
             val x = buildL(ns, b)
             val fm = b.getClass.getMethod(t.head.toLowerCase(), asScalaBuffer(new java.util.ArrayList[Class[_]]):_*)
             refArrayOps(fm.invoke(null).getClass.getMethods) foreach { m =>
               if (m.getName.equals(t.label.toLowerCase)) 
                 try
                   return invokeMethod(m, x, t.head, t.label)
                 catch { 
                   case e: IllegalArgumentException =>
                   case e: InvocationTargetException =>
                 }
             }
             throw new IllegalArgumentException(composeErrorMsg(t.head, t.label, x))
           }
      case Star(ns)  => skip(buildL(ns, b, flatten = true))
      case Plus(ns)  => skip(buildL(ns, b, flatten = true))
      case Opt(n)    => val x = build(n, b)
                        if (x.isInstanceOf[java.util.List[_]])
                          return x
                        else {
                          val y = new java.util.ArrayList[Object]
                          if (x == None)
                            return y
                          y.add(x)
                          return y
                        }
      case Group(ns) => skip(buildL(ns, b, flatten = true))
        
      case TerminalTerm(_, i, j, input) => return input.subString(i, j)
      case Epsilon(_) => None
      
      case _ => throw new RuntimeException("Unexpected type of a term: " + term)
    }
    
  }

  /**
    * Skips layout and flattens immediate list children
    */
  def buildL(children: Seq[Term], b: Actions, flatten: Boolean = false) = {
    val builder = new scala.collection.mutable.ListBuffer[Object]
    val result = new java.util.ArrayList[Object];
    children foreach { child =>
      if (!isLayout(child)) {
        val x = build(child, b)
        if (x.isInstanceOf[List[_]] && flatten)
          builder ++= x.asInstanceOf[List[Object]]
        else
          builder += x
      }
    }
    builder.toList
  }

  /**
    * Skips terminals unless there are only terminals in the list
    */
  def skip(l: List[Object]): List[Object] = {
    if (l.isEmpty || l.size == 1) return l
    val x = l filter {!_.isInstanceOf[String]}
    if (x.isEmpty) return l;
    x
  }

  def isLayout(term: Term): Boolean = {
    if (term.isInstanceOf[NonterminalTerm])
      term.asInstanceOf[NonterminalTerm].isLayout
    else if (term.isInstanceOf[TerminalTerm])
      term.asInstanceOf[TerminalTerm].isLayout
    else
      false;
  }

  /**
    * Attempts to match the signature of a method (@param m) with arguments (@param args):
    *     - if the number of arguments matches the number of parameters, tries to invoke the method
    *     - if the number of arguments does not match (greater than) the number of parameters, tries to skip terminals
    *       following the order of parameters and their declared types; if a terminal is expected, pass it,
    *       otherwise skip it; one special case of trailing terminals in the argument list has to be taken care of
    */
  def invokeMethod(m: java.lang.reflect.Method, args: List[Object], head: String, label: String): Object = {
    if (m.getParameterCount == args.size)
      return m.invoke(null, args: _*)
    else {
      val types = m.getParameterTypes;
      
      if (types.length > args.size) 
        throw new IllegalArgumentException(composeErrorMsg(head, label, args))
      
      val y = new scala.collection.mutable.ListBuffer[Object]
      var i = 0
      var len = args.size
      
      args foreach { elem =>
        
        if (i > types.length) 
          throw new IllegalArgumentException(composeErrorMsg(head, label, args))
        
        if (elem.isInstanceOf[java.lang.String] && (types(i) != elem.getClass) || i == types.length) {
          len -= 1;
          if (len < types.length) 
            throw new IllegalArgumentException(composeErrorMsg(head, label, args))
        } else {
          y += elem
          i += 1
        }
      }
      
      if (y.size != types.length) 
        throw new IllegalArgumentException(composeErrorMsg(head, label, args))
      
      return m.invoke(null, y: _*)
    }
  }

  def composeErrorMsg(head: String, label: String, args: List[Object])
    = "The matching method has not been found: $head . ${label.toLowerCase} (${args.foldLeft(\"\") {(a,el) => a + \",\" + el.getClass.getName}})"
  
}