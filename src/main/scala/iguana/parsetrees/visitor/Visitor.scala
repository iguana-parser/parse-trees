/*
 * Copyright (c) 2015,  Ali Afroozeh and Anastasia Izmaylova, Centrum Wiskunde & Informatica (CWI)
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

package iguana.parsetrees.visitor

import iguana.parsetrees.sppf.{IntermediateNode, NonterminalNode, SPPFNode}

import scala.collection.mutable._

trait Visitor[A] {
  type T
  def visit(node: A): VisitResult[T]
}

trait VisitResult[+A] {

  def get: A

  def isDefined: Boolean

  def isEmpty: Boolean = !isDefined

  def map[B](f: A => B): VisitResult[B] =
    if (isDefined) Some(f(this.get)) else None

  def flatMap[B](f: A => VisitResult[B]): VisitResult[B] =
    if (isDefined) f(this.get) else None

  def toSeq: collection.Seq[A] =
    if (isDefined) Buffer(this.get) else Buffer()
}

case class Some[+A](a: A) extends VisitResult[A] {
  override def get: A = a
  override def isDefined: Boolean = true
}

case object None extends VisitResult[Nothing] {
  override def get: Nothing = throw new NoSuchElementException
  override def isDefined: Boolean = false
}

case class Unknown(node: Any) extends VisitResult[Nothing] {
  override def get: Nothing = throw new NoSuchElementException
  override def isDefined: Boolean = false
  def getNode: Any = node
}

trait Id {
  val ids = new HashMap[Any, Int]
  def getId(node: Any): Int = ids.getOrElseUpdate(node, ids.size)
}

trait Memoization[A] extends Visitor[A] {

  val cache = new HashMap[A, VisitResult[T]]

  override abstract def visit(node: A): VisitResult[T] = cache.getOrElseUpdate(node, super.visit(node))
}

trait SPPFMemoization extends Visitor[SPPFNode] {
  val cache = new HashMap[SPPFNode, VisitResult[T]]

  override abstract def visit(node: SPPFNode): VisitResult[T] = {

    node match {

      case n: NonterminalNode => {
        if (!cache.contains(node)) {
          cache.put(node, Unknown(n))
          val v = super.visit(node)
          cache.put(node, v)
          return v
        } else {
          cache.get(node).get
        }
      }

      case _ => cache.getOrElseUpdate(node, super.visit(node))
    }
  }
}

trait Predicate[A] extends Visitor[A] {

  def predicate: A => Boolean

  override abstract def visit(node: A): VisitResult[T] = {
    if (predicate(node)) super.visit(node) else None
  }

}