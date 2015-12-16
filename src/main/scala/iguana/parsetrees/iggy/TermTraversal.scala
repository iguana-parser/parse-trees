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
      case NonterminalTerm(t, ns, _) => if (t.label == null || t.label.isEmpty()) {
                                   if (ns.size == 1) return build(ns.head, b);
                                   val x = skip(buildL(ns, b))
                                   if (x.size() == 1) return x.get(0)
                                   else
                                     throw new RuntimeException("This rule should have a label: " + t)
                                 } else if (t.label.toLowerCase.equals("start")) {
                                   return build(ns.tail.head, b)
                                 } else {
                                   val x = buildL(ns, b)
                                   refArrayOps(b.getClass.getMethod(t.head.toLowerCase(), asScalaBuffer(new java.util.ArrayList[Class[_]]):_*)
                                               .invoke(null).getClass.getMethods) foreach { m => // Public, static method!
                                     if (m.getName.equals(t.label.toLowerCase)) {
                                       if (m.getParameterCount == x.size())
                                         try {
                                           return m.invoke(null, x: _*)
                                         } catch {
                                           case e: IllegalArgumentException =>
                                             throw new RuntimeException("The matching method has not been found: "
                                                         + t.head + "." + t.label.toLowerCase + x.foldLeft("") { (a, el) => a + "," + el.getClass.getName })
                                           case e: InvocationTargetException => throw e.getTargetException
                                         }
                                       else {
                                         val types = m.getParameterTypes;
                                         if (types.length > x.size())
                                           throw new RuntimeException("The matching method has not been found: "
                                                       + t.head + "." + t.label.toLowerCase + x.foldLeft("") { (a, el) => a + "," + el.getClass.getName })
                                         val y = new java.util.ArrayList[Object]
                                         var i, j = 0
                                         var len = x.size
                                         asScalaBuffer(x) foreach { elem =>
                                           if (j > types.length)
                                             throw new RuntimeException("The matching method has not been found: "
                                               + t.head + "." + t.label.toLowerCase + x.foldLeft("") { (a, el) => a + "," + el.getClass.getName })
                                           if (elem.isInstanceOf[java.lang.String]
                                                   && (j == types.length || types.apply(j) != elem.getClass)) {
                                             len = len - 1;
                                             if (len < types.length)
                                               throw new RuntimeException("The matching method has not been found: "
                                                           + t.head + "." + t.label.toLowerCase + x.foldLeft("") { (a, el) => a + "," + el.getClass.getName })
                                           } else {
                                             y.add(elem)
                                             j = j + 1
                                           }
                                           i = i + 1
                                         }
                                         if (y.size != types.length)
                                           throw new RuntimeException("The matching method has not been found: "
                                                       + t.head + "." + t.label.toLowerCase + x.foldLeft("") { (a, el) => a + "," + el.getClass.getName })
                                         try {
                                           return m.invoke(null, y: _*)
                                         } catch {
                                           case e: IllegalArgumentException =>
                                             throw new RuntimeException("The matching method has not been found: "
                                                     + t.head + "." + t.label.toLowerCase + x.foldLeft("") { (a, el) => a + "," + el.getClass.getName })
                                           case e: InvocationTargetException => throw e.getTargetException
                                         }
                                       }
                                     }
                                   }
                                   throw new RuntimeException("The matching method has not been found: "
                                                 + t.head + "." + t.label.toLowerCase + x.foldLeft(""){ (a, el) => a + "," + el.getClass.getName })
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
  
  def buildL(children: Seq[Term], b: Actions, flatten: Boolean = false): java.util.List[Object] = {
    val l = new java.util.ArrayList[Object];
    var i = 0
    children foreach { child =>
      if (i==0||i%2==0) { // Skip layout inserted between symbols
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
  
  def skip(l: java.util.List[Object]): java.util.List[Object] = {
    if (l.isEmpty() || l.size() == 1) return l
    val x = new java.util.ArrayList[Object]
    asScalaBuffer(l) foreach { elem =>
      if (!elem.isInstanceOf[java.lang.String])
        x.add(elem)
    }
    x
  }
  
}