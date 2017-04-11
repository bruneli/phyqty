package com.github.bruneli.phyqty
/*
 * Copyright 2016 Renaud Bruneliere
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import scala.collection.{IndexedSeq, IndexedSeqLike, Traversable}
import scala.collection.{SeqView, SeqViewLike, TraversableView}
import Dimension.{/, x}

import scala.collection.TraversableView.NoBuilder
import scala.collection.generic.{CanBuildFrom, SliceInterval}
import scala.collection.generic.SliceInterval._

/**
  * @author bruneli
  */
trait QuantitiesView[D <: Dimension[_, _, _, _, _, _, _], Coll] extends IndexedSeq[Quantity[D]]
  with IndexedSeqLike[Quantity[D], QuantitiesView[D, Coll]]
  with SeqViewLike[Quantity[D], Coll, QuantitiesView[D, Coll]]
  with QuantitiesLike[D]
  with SeqView[Quantity[D], Coll] {
  self =>

  private[this] type This = QuantitiesView[D, Coll]

  trait Transformed[B <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesView[B, Coll] with super.Transformed[Quantity[B]] {
    override def toString = viewToString
  }

  /** Explicit instantiation of the `Transformed` trait to reduce class file size in subclasses. */
  private abstract class AbstractTransformed[B <: Dimension[_, _, _, _, _, _, _]] extends Transformed[B]

  override def apply(idx: Int): Quantity[D] = Quantity(magnitude(idx), unit)

  override def force: Quantities[D] = {
    val magnitudes = new Array[Double](length)
    for (idx <- magnitudes.indices) {
      magnitudes(idx) = magnitude(idx)
    }
    Quantities(magnitudes, unit)
  }

  override def mapMagnitudes(g: Double => Double): QuantitiesView[D, Quantities[D]] = {
    new MappingOperation {
      val f = g
    }
  }

  override def + (that: QuantitiesLike[D]): QuantitiesView[D, Quantities[D]] = {
    new VectorAddition {
      val other = that
    }
  }

  override def + (that: Quantity[D]): QuantitiesView[D, Quantities[D]] = {
    new ConstantAddition {
      val constant = that
    }
  }

  override def - (that: QuantitiesLike[D]): QuantitiesView[D, Quantities[D]] = {
    new VectorSubtraction {
      val other = that
    }
  }

  override def - (that: Quantity[D]): QuantitiesView[D, Quantities[D]] = {
    new ConstantSubtraction {
      val constant = that
    }
  }

  override def unary_-(): QuantitiesView[D, Quantities[D]] = {
    new MappingOperation {
      override val f = (x: Double) => -x
    }
  }

  override def * [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD]): QuantitiesView[D x DD, Quantities[D x DD]] = {
    new VectorMultiplication[DD] {
      val other = that
    }
  }

  override def * [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD]): QuantitiesView[D x DD, Quantities[D x DD]] = {
    new ConstantMultiplication[DD] {
      val constant = that
    }
  }

  override def * (scalar: Double): QuantitiesView[D, Quantities[D]] = {
    new MappingOperation {
      override val f = (x: Double) => x * scalar
    }
  }

  override def / [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD]): QuantitiesView[D / DD, Quantities[D / DD]] = {
    new VectorDivision[DD] {
      val other = that
    }
  }

  override def / [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD]): QuantitiesView[D / DD, Quantities[D / DD]] = {
    new ConstantDivision[DD] {
      val constant = that
    }
  }

  override def / (scalar: Double): QuantitiesView[D, Quantities[D]] = {
    new MappingOperation {
      override val f = (x: Double) => x / scalar
    }
  }

  override def in(anotherUnit: PhyUnit[D]): QuantitiesView[D, Quantities[D]] = {
    new ConversionOperation {
      override val thatUnit = anotherUnit
    }
  }

  override def diff(idx: Int): QuantitiesView[D, Quantities[D]] = {
    new DiffOperation {
      override val offset = idx
    }
  }

  protected override def newFiltered(p: Quantity[D] => Boolean): Transformed[D] = {
    new { val pred = p } with AbstractTransformed[D] with Filtered
  }
  protected def newSliced(from: Int, until: Int): Transformed[D] = {
    new { val endpoints = SliceInterval(from, until) } with AbstractTransformed[D] with Sliced
  }
  protected override def newDroppedWhile(p: Quantity[D] => Boolean): Transformed[D] = {
    new { val pred = p } with AbstractTransformed[D] with DroppedWhile
  }
  protected override def newTakenWhile(p: Quantity[D] => Boolean): Transformed[D] = {
    new { val pred = p } with AbstractTransformed[D] with TakenWhile
  }
  protected override def newReversed: Transformed[D] = new AbstractTransformed[D] with Reversed

  override def init: This = newSliced(0, self.length - 1)
  override def drop(n: Int): This = newSliced(n, self.length)
  override def take(n: Int): This = newSliced(0, n min self.length)
  override def slice(from: Int, until: Int): This = newSliced(from, until min self.length)
  override def dropWhile(p: Quantity[D] => Boolean): This = newDroppedWhile(p)
  override def takeWhile(p: Quantity[D] => Boolean): This = newTakenWhile(p)
  override def span(p: Quantity[D] => Boolean): (This, This) = (newTakenWhile(p), newDroppedWhile(p))
  override def splitAt(n: Int): (This, This) = (take(n), drop(n)) // !!!
  override def reverse: This = newReversed
  override def tail: QuantitiesView[D, Coll] = if (isEmpty) super.tail else slice(1, length)
  override def stringPrefix = "QuantitiesView"

  trait QuantitiesOperation[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesView[DD, Quantities[DD]] {
    protected lazy val underlying = ???
    override def length = self.length
    override def apply(idx: Int) = Quantity(magnitude(idx), unit)
  }

  trait MappingOperation extends QuantitiesOperation[D] {
    val f: Double => Double
    val unit = self.unit
    override def magnitude(idx: Int) = {
      f(self.magnitude(idx))
    }
  }

  trait VectorAddition extends QuantitiesOperation[D] {
    val other: QuantitiesLike[D]
    val unit = self.unit
    override def magnitude(idx: Int) = {
      self.magnitude(idx) + converter(other.unit)(other.magnitude(idx))
    }
  }

  trait ConstantAddition extends QuantitiesOperation[D] {
    val constant: Quantity[D]
    val unit = self.unit
    override def magnitude(idx: Int) = {
      self.magnitude(idx) + converter(constant.unit)(constant.magnitude)
    }
  }

  trait VectorSubtraction extends QuantitiesOperation[D] {
    val other: QuantitiesLike[D]
    val unit = self.unit
    override def magnitude(idx: Int) = {
      self.magnitude(idx) - converter(other.unit)(other.magnitude(idx))
    }
  }

  trait ConstantSubtraction extends QuantitiesOperation[D] {
    val constant: Quantity[D]
    val unit = self.unit
    override def magnitude(idx: Int) = {
      self.magnitude(idx) - converter(constant.unit)(constant.magnitude)
    }
  }

  trait VectorMultiplication[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesOperation[D x DD] {
    val other: QuantitiesLike[DD]
    lazy val unit = self.unit * other.unit
    override def magnitude(idx: Int) = {
      self.magnitude(idx) * other.magnitude(idx)
    }
  }

  trait ConstantMultiplication[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesOperation[D x DD] {
    val constant: Quantity[DD]
    lazy val unit = self.unit * constant.unit
    override def magnitude(idx: Int) = {
      self.magnitude(idx) * constant.magnitude
    }
  }

  trait VectorDivision[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesOperation[D / DD] {
    val other: QuantitiesLike[DD]
    lazy val unit = self.unit / other.unit
    override def magnitude(idx: Int) = {
      self.magnitude(idx) / other.magnitude(idx)
    }
  }

  trait ConstantDivision[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesOperation[D / DD] {
    val constant: Quantity[DD]
    lazy val unit = self.unit / constant.unit
    override def magnitude(idx: Int) = {
      self.magnitude(idx) / constant.magnitude
    }
  }

  trait ConversionOperation extends QuantitiesOperation[D] {
    val unit = self.unit
    val thatUnit: PhyUnit[D]
    override def magnitude(idx: Int) = {
      thatUnit.converter.inverse(self.magnitude(idx), unit.converter)
    }
  }

  trait DiffOperation extends QuantitiesOperation[D] {
    val unit = self.unit
    val offset: Int
    override def magnitude(idx: Int) = {
      if (offset > 0 && idx < offset || offset < 0 && idx > length - offset) {
        Double.NaN
      } else {
        self.magnitude(idx) - self.magnitude(idx - offset)
      }
    }
  }

  trait Sliced extends super.Sliced with Transformed[D] {
    override def length = endpoints.width
    val unit = self.unit
    override def magnitude(idx: Int) = {
      if (idx >= 0 && idx + from < until) self.magnitude(idx + from)
      else throw new IndexOutOfBoundsException(idx.toString)
    }
  }

  trait Filtered extends super.Filtered with Transformed[D] {
    val unit = self.unit
    def magnitude(idx: Int) = self.magnitude(index(idx))
  }

  trait TakenWhile extends super.TakenWhile with Transformed[D] {
    val unit = self.unit
    override def magnitude(idx: Int) =
      if (idx < len) self.magnitude(idx)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait DroppedWhile extends super.DroppedWhile with Transformed[D] {
    val unit = self.unit
    override def magnitude(idx: Int) =
      if (idx >= 0) self.magnitude(idx + start)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait Reversed extends super.Reversed with Transformed[D] {
    val unit = self.unit
    override def magnitude(idx: Int) = self.magnitude(self.length - 1 - idx)
  }

}

object QuantitiesView {
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, SeqView[A, Seq[_]]] =
    new CanBuildFrom[Coll, A, SeqView[A, Seq[_]]] {
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
  implicit def arrCanBuildFrom[A]: CanBuildFrom[TraversableView[_, Array[_]], A, SeqView[A, Array[A]]] =
    new CanBuildFrom[TraversableView[_, Array[_]], A, SeqView[A, Array[A]]] {
      def apply(from: TraversableView[_, Array[_]]) = new NoBuilder
      def apply() = new NoBuilder
    }
//  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
//  implicit def canBuildFrom[A <: Dimension[_, _, _, _, _, _, _]]: CanBuildFrom[Coll, Quantity[A], QuantitiesView[A, Quantities[A]]] =
//    new CanBuildFrom[Coll, Quantity[A], QuantitiesView[A, Quantities[A]]] {
//      def apply(from: Coll) = new NoBuilder
//      def apply() = new NoBuilder
//    }
//  implicit def arrCanBuildFrom[A <: Dimension[_, _, _, _, _, _, _]]: CanBuildFrom[TraversableView[_, Array[_]], Quantity[A], QuantitiesView[A, Quantities[A]]] =
//    new CanBuildFrom[TraversableView[_, Array[_]], Quantity[A], QuantitiesView[A, Quantities[A]]] {
//      def apply(from: TraversableView[_, Array[_]]) = new NoBuilder
//      def apply() = new NoBuilder
//    }
}