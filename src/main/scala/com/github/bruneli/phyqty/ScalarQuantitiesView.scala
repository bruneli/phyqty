package com.github.bruneli.phyqty

/*
 * Copyright 2017 Renaud Bruneliere
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

import com.github.bruneli.phyqty.Dimension.{/, x}

import scala.collection.TraversableView.NoBuilder
import scala.collection.generic.{CanBuildFrom, SliceInterval}
import scala.collection.{IndexedSeq, IndexedSeqLike, SeqView, SeqViewLike, Traversable, TraversableView}

/**
  * @author bruneli
  */
trait ScalarQuantitiesView[D <: Dimension[_, _, _, _, _, _, _], Coll] extends IndexedSeq[ScalarQuantity[D]]
  with IndexedSeqLike[ScalarQuantity[D], ScalarQuantitiesView[D, Coll]]
  with SeqViewLike[ScalarQuantity[D], Coll, ScalarQuantitiesView[D, Coll]]
  with QuantitiesLike[D, Scalar]
  with SeqView[ScalarQuantity[D], Coll] {
  self =>

  private[this] type This = ScalarQuantitiesView[D, Coll]

  override def force: ScalarQuantities[D] = {
    val magnitudes = new Array[Double](length)
    for (idx <- magnitudes.indices) {
      magnitudes(idx) = magnitude(idx)
    }
    ScalarQuantities(magnitudes, unit)
  }

  trait Transformed[B <: Dimension[_, _, _, _, _, _, _]] extends ScalarQuantitiesView[B, Coll] with super.Transformed[ScalarQuantity[B]] {
    override def toString = viewToString
  }

  /** Explicit instantiation of the `Transformed` trait to reduce class file size in subclasses. */
  private abstract class AbstractTransformed[B <: Dimension[_, _, _, _, _, _, _]] extends Transformed[B]

  override def apply(idx: Int): ScalarQuantity[D] = ScalarQuantity(magnitude(idx), unit)

  override def magnitude(idx: Int): Double = coordinate(0, idx)

  override def mapCoordinates(g: Double => Double): ScalarQuantitiesView[D, Coll] = {
    new MappingOperation {
      val f = g
    }
  }

  override def + (that: QuantitiesLike[D, Scalar]): ScalarQuantitiesView[D, Coll] = {
    new VectorAddition {
      val other = that
    }
  }

  override def + (that: Quantity[D, Scalar]): ScalarQuantitiesView[D, Coll] = {
    new ConstantAddition {
      val constant = that
    }
  }

  override def - (that: QuantitiesLike[D, Scalar]): ScalarQuantitiesView[D, Coll] = {
    new VectorSubtraction {
      val other = that
    }
  }

  override def - (that: Quantity[D, Scalar]): ScalarQuantitiesView[D, Coll] = {
    new ConstantSubtraction {
      val constant = that
    }
  }

  override def unary_-(): ScalarQuantitiesView[D, Coll] = {
    new MappingOperation {
      override val f = (x: Double) => -x
    }
  }

  override def * [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD, Scalar]): ScalarQuantitiesView[D x DD, Coll] = {
    new VectorMultiplication[DD] {
      val other = that
    }
  }

  override def * [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD, Scalar]): ScalarQuantitiesView[D x DD, Coll] = {
    new ConstantMultiplication[DD] {
      val constant = that
    }
  }

  override def * (scalar: Double): ScalarQuantitiesView[D, Coll] = {
    new MappingOperation {
      override val f = (x: Double) => x * scalar
    }
  }

  override def / [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD, Scalar]): ScalarQuantitiesView[D / DD, Coll] = {
    new VectorDivision[DD] {
      val other = that
    }
  }

  override def / [DD <: Dimension[_, _, _, _, _, _, _]](that: ScalarQuantity[DD]): ScalarQuantitiesView[D / DD, Coll] = {
    new ConstantDivision[DD] {
      val constant = that
    }
  }

  override def / (scalar: Double): ScalarQuantitiesView[D, Coll] = {
    new MappingOperation {
      override val f = (x: Double) => x / scalar
    }
  }

  override def in(anotherUnit: PhyUnit[D]): ScalarQuantitiesView[D, Coll] = {
    new ConversionOperation {
      override val thatUnit = anotherUnit
    }
  }

  override def diff(idx: Int): ScalarQuantitiesView[D, Coll] = {
    new DiffOperation {
      override val offset = idx
    }
  }

  override protected def buildQuantity[DD <: Dimension[_, _, _, _, _, _, _]](coordinates: Array[Double], unit: PhyUnit[DD]): ScalarQuantity[DD] = {
    ScalarQuantity(coordinates(0), unit)
  }

  protected override def newFiltered(p: ScalarQuantity[D] => Boolean): Transformed[D] = {
    new { val pred = p } with AbstractTransformed[D] with Filtered
  }
  protected def newSliced(from: Int, until: Int): Transformed[D] = {
    new { val endpoints = SliceInterval(from, until) } with AbstractTransformed[D] with Sliced
  }
  protected override def newDroppedWhile(p: ScalarQuantity[D] => Boolean): Transformed[D] = {
    new { val pred = p } with AbstractTransformed[D] with DroppedWhile
  }
  protected override def newTakenWhile(p: ScalarQuantity[D] => Boolean): Transformed[D] = {
    new { val pred = p } with AbstractTransformed[D] with TakenWhile
  }
  protected override def newReversed: Transformed[D] = new AbstractTransformed[D] with Reversed

  override def init: This = newSliced(0, self.length - 1)
  override def drop(n: Int): This = newSliced(n, self.length)
  override def take(n: Int): This = newSliced(0, n min self.length)
  override def slice(from: Int, until: Int): This = newSliced(from, until min self.length)
  override def dropWhile(p: ScalarQuantity[D] => Boolean): This = newDroppedWhile(p)
  override def takeWhile(p: ScalarQuantity[D] => Boolean): This = newTakenWhile(p)
  override def span(p: ScalarQuantity[D] => Boolean): (This, This) = (newTakenWhile(p), newDroppedWhile(p))
  override def splitAt(n: Int): (This, This) = (take(n), drop(n)) // !!!
  override def reverse: This = newReversed
  override def tail: ScalarQuantitiesView[D, Coll] = if (isEmpty) super.tail else slice(1, length)
  override def stringPrefix = "ScalarQuantitiesView"

  trait QuantitiesOperation[DD <: Dimension[_, _, _, _, _, _, _]] extends ScalarQuantitiesView[DD, Coll] {
    protected lazy val underlying = ???
    override def length = self.length
    val dimension = self.dimension
    override def apply(idx: Int) = ScalarQuantity[DD](magnitude(idx), unit)
  }

  trait MappingOperation extends QuantitiesOperation[D] {
    val f: Double => Double
    val unit = self.unit
    override def coordinate(i: Int, j: Int): Double = {
      f(self.coordinate(i, j))
    }
  }

  trait VectorAddition extends QuantitiesOperation[D] {
    val other: QuantitiesLike[D, Scalar]
    val unit = self.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) + converter(other.unit)(other.coordinate(i, j))
    }
  }

  trait ConstantAddition extends QuantitiesOperation[D] {
    val constant: Quantity[D, Scalar]
    val unit = self.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) + converter(constant.unit)(constant.coordinate(j))
    }
  }

  trait VectorSubtraction extends QuantitiesOperation[D] {
    val other: QuantitiesLike[D, Scalar]
    val unit = self.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) - converter(other.unit)(other.coordinate(i, j))
    }
  }

  trait ConstantSubtraction extends QuantitiesOperation[D] {
    val constant: Quantity[D, Scalar]
    val unit = self.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) - converter(constant.unit)(constant.coordinate(j))
    }
  }

  trait VectorMultiplication[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesOperation[D x DD] {
    val other: QuantitiesLike[DD, Scalar]
    lazy val unit = self.unit * other.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) * other.coordinate(i, j)
    }
  }

  trait ConstantMultiplication[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesOperation[D x DD] {
    val constant: Quantity[DD, Scalar]
    lazy val unit = self.unit * constant.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) * constant.coordinate(j)
    }
  }

  trait VectorDivision[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesOperation[D / DD] {
    val other: QuantitiesLike[DD, Scalar]
    lazy val unit = self.unit / other.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) / other.coordinate(0, j)
    }
  }

  trait ConstantDivision[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesOperation[D / DD] {
    val constant: ScalarQuantity[DD]
    lazy val unit = self.unit / constant.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) / constant.coordinate(0)
    }
  }

  trait ConversionOperation extends QuantitiesOperation[D] {
    val unit = self.unit
    val thatUnit: PhyUnit[D]
    override def coordinate(i: Int, j: Int): Double = {
      thatUnit.converter.inverse(self.coordinate(i, j), unit.converter)
    }
  }

  trait DiffOperation extends QuantitiesOperation[D] {
    val unit = self.unit
    val offset: Int
    override def coordinate(i: Int, j: Int): Double = {
      if (offset > 0 && j < offset || offset < 0 && j > length - offset) {
        Double.NaN
      } else {
        self.coordinate(i, j) - self.coordinate(i, j - offset)
      }
    }
  }

  trait Sliced extends super.Sliced with Transformed[D] {
    override def length = endpoints.width
    val unit = self.unit
    val dimension = self.dimension
    override def coordinate(i: Int, j: Int): Double = {
      if (i >= 0 && i + from < until) self.coordinate(i + from, j)
      else throw new IndexOutOfBoundsException(i.toString)
    }
  }

  trait Filtered extends super.Filtered with Transformed[D] {
    val unit = self.unit
    val dimension = self.dimension
    override def coordinate(i: Int, j: Int): Double = self.coordinate(index(i), j)
  }

  trait TakenWhile extends super.TakenWhile with Transformed[D] {
    val unit = self.unit
    val dimension = self.dimension
    override def coordinate(i: Int, j: Int): Double =
      if (i < len) self.coordinate(i, j)
      else throw new IndexOutOfBoundsException(i.toString)
  }

  trait DroppedWhile extends super.DroppedWhile with Transformed[D] {
    val unit = self.unit
    val dimension = self.dimension
    override def coordinate(i: Int, j: Int): Double =
      if (i >= 0) self.coordinate(i + start, j)
      else throw new IndexOutOfBoundsException(i.toString)
  }

  trait Reversed extends super.Reversed with Transformed[D] {
    val unit = self.unit
    val dimension = self.dimension
    override def coordinate(i: Int, j: Int): Double =
      self.coordinate(self.length - 1 - i, j)
  }

}

object ScalarQuantitiesView {
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