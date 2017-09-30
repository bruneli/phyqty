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
trait VectorQuantitiesView[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType, Coll]
  extends IndexedSeq[VectorQuantity[D, N]]
    with IndexedSeqLike[VectorQuantity[D, N], VectorQuantitiesView[D, N, Coll]]
    with SeqViewLike[VectorQuantity[D, N], Coll, VectorQuantitiesView[D, N, Coll]]
    with QuantitiesLike[D, N]
    with SeqView[VectorQuantity[D, N], Coll] {
  self =>

  private[this] type This = VectorQuantitiesView[D, N, Coll]

  override def force: VectorQuantities[D, N] = {
    val coordinates = new Array[Double](length * dimension)
    var k = 0
    for {i <- 0 until dimension
         j <- 0 until length} {
      coordinates(k) = this.coordinate(i, j)
      k += 1
    }
    new VectorQuantities[D, N](coordinates, unit, dimension)
  }

  trait Transformed[B <: Dimension[_, _, _, _, _, _, _]] extends VectorQuantitiesView[B, N, Coll] with super.Transformed[VectorQuantity[B, N]] {
    override def toString = viewToString
  }

  /** Explicit instantiation of the `Transformed` trait to reduce class file size in subclasses. */
  private abstract class AbstractTransformed[B <: Dimension[_, _, _, _, _, _, _]] extends Transformed[B]

  override def apply(j: Int): VectorQuantity[D, N] = {
    val coordinates = new Array[Double](dimension)
    for (i <- 0 until dimension) {
      coordinates(i) = this.coordinate(i, j)
    }
    VectorQuantity(coordinates, unit)
  }

  override def magnitude(j: Int): Double = {
    var sum = 0.0
    for (i <- 0 until dimension) {
      sum += coordinate(i, j) * coordinate(i, j)
    }
    math.sqrt(sum)
  }

  override def mapCoordinates(g: Double => Double): VectorQuantitiesView[D, N, Coll] = {
    new MappingOperation {
      val f = g
    }
  }

  override def + (that: QuantitiesLike[D, N]): VectorQuantitiesView[D, N, Coll] = {
    new VectorAddition {
      val other = that
    }
  }

  override def + (that: Quantity[D, N]): VectorQuantitiesView[D, N, Coll] = {
    new ConstantAddition {
      val constant = that
    }
  }

  override def - (that: QuantitiesLike[D, N]): VectorQuantitiesView[D, N, Coll] = {
    new VectorSubtraction {
      val other = that
    }
  }

  override def - (that: Quantity[D, N]): VectorQuantitiesView[D, N, Coll] = {
    new ConstantSubtraction {
      val constant = that
    }
  }

  override def unary_-(): VectorQuantitiesView[D, N, Coll] = {
    new MappingOperation {
      override val f = (x: Double) => -x
    }
  }

  override def * [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD, N]): ScalarQuantitiesView[D x DD, Coll] = {
    new VectorMultiplication[DD] {
      val other = that
    }
  }

  override def * [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD, N]): ScalarQuantitiesView[D x DD, Coll] = {
    new ConstantMultiplication[DD] {
      val constant = that
      val initialDimension = self.dimension
    }
  }

  override def * (scalar: Double): VectorQuantitiesView[D, N, Coll] = {
    new MappingOperation {
      override val f = (x: Double) => x * scalar
    }
  }

  override def / [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD, Scalar]): VectorQuantitiesView[D / DD, N, Coll] = {
    new VectorDivision[DD] {
      val other = that
    }
  }

  override def / [DD <: Dimension[_, _, _, _, _, _, _]](that: ScalarQuantity[DD]): VectorQuantitiesView[D / DD, N, Coll] = {
    new ConstantDivision[DD] {
      val constant = that
    }
  }

  override def / (scalar: Double): VectorQuantitiesView[D, N, Coll] = {
    new MappingOperation {
      override val f = (x: Double) => x / scalar
    }
  }

  override def in(anotherUnit: PhyUnit[D]): VectorQuantitiesView[D, N, Coll] = {
    new ConversionOperation {
      override val unit = anotherUnit
    }
  }

  override def diff(idx: Int): VectorQuantitiesView[D, N, Coll] = {
    new DiffOperation {
      override val offset = idx
    }
  }

  override protected def buildQuantity[DD <: Dimension[_, _, _, _, _, _, _]](coordinates: Array[Double], unit: PhyUnit[DD]): VectorQuantity[DD, N] = {
    new VectorQuantity[DD, N](coordinates, unit)
  }

  protected override def newFiltered(p: VectorQuantity[D, N] => Boolean): Transformed[D] = {
    new { val pred = p } with AbstractTransformed[D] with Filtered
  }
  protected def newSliced(from: Int, until: Int): Transformed[D] = {
    new { val endpoints = SliceInterval(from, until) } with AbstractTransformed[D] with Sliced
  }
  protected override def newDroppedWhile(p: VectorQuantity[D, N] => Boolean): Transformed[D] = {
    new { val pred = p } with AbstractTransformed[D] with DroppedWhile
  }
  protected override def newTakenWhile(p: VectorQuantity[D, N] => Boolean): Transformed[D] = {
    new { val pred = p } with AbstractTransformed[D] with TakenWhile
  }
  protected override def newReversed: Transformed[D] = new AbstractTransformed[D] with Reversed

  override def init: This = newSliced(0, self.length - 1)
  override def drop(n: Int): This = newSliced(n, self.length)
  override def take(n: Int): This = newSliced(0, n min self.length)
  override def slice(from: Int, until: Int): This = newSliced(from, until min self.length)
  override def dropWhile(p: VectorQuantity[D, N] => Boolean): This = newDroppedWhile(p)
  override def takeWhile(p: VectorQuantity[D, N] => Boolean): This = newTakenWhile(p)
  override def span(p: VectorQuantity[D, N] => Boolean): (This, This) = (newTakenWhile(p), newDroppedWhile(p))
  override def splitAt(n: Int): (This, This) = (take(n), drop(n)) // !!!
  override def reverse: This = newReversed
  override def tail: VectorQuantitiesView[D, N, Coll] = if (isEmpty) super.tail else slice(1, length)
  override def stringPrefix = "VectorQuantitiesView"

  trait VectorQuantitiesOperation[DD <: Dimension[_, _, _, _, _, _, _]] extends VectorQuantitiesView[DD, N, Coll] {
    protected lazy val underlying = ???
    override def length = self.length
    val dimension = self.dimension
    override def apply(j: Int) = {
      val coordinates = new Array[Double](dimension)
      for (i <- 0 until dimension) {
        coordinates(i) = this.coordinate(i, j)
      }
      new VectorQuantity[DD, N](coordinates, unit)
    }
  }

  trait ScalarQuantitiesOperation[DD <: Dimension[_, _, _, _, _, _, _]] extends ScalarQuantitiesView[DD, Coll] {
    protected lazy val underlying = ???
    override def length = self.length
    val dimension = 1
    override def apply(idx: Int) = ScalarQuantity[DD](magnitude(idx), unit)
  }

  trait MappingOperation extends VectorQuantitiesOperation[D] {
    val f: Double => Double
    val unit = self.unit
    override def coordinate(i: Int, j: Int): Double = {
      f(self.coordinate(i, j))
    }
  }

  trait VectorAddition extends VectorQuantitiesOperation[D] {
    val other: QuantitiesLike[D, N]
    val unit = self.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) + converter(other.unit)(other.coordinate(i, j))
    }
  }

  trait ConstantAddition extends VectorQuantitiesOperation[D] {
    val constant: Quantity[D, N]
    val unit = self.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) + converter(constant.unit)(constant.coordinate(i))
    }
  }

  trait VectorSubtraction extends VectorQuantitiesOperation[D] {
    val other: QuantitiesLike[D, N]
    val unit = self.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) - converter(other.unit)(other.coordinate(i, j))
    }
  }

  trait ConstantSubtraction extends VectorQuantitiesOperation[D] {
    val constant: Quantity[D, N]
    val unit = self.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) - converter(constant.unit)(constant.coordinate(i))
    }
  }

  trait VectorMultiplication[DD <: Dimension[_, _, _, _, _, _, _]] extends ScalarQuantitiesOperation[D x DD] {
    val other: QuantitiesLike[DD, N]
    lazy val unit = self.unit * other.unit
    override def coordinate(i: Int, j: Int): Double = {
      var sum = 0.0
      for (i <- 0 until other.dimension) {
        sum += self.coordinate(i, j) * other.coordinate(i, j)
      }
      sum
    }
  }

  trait ConstantMultiplication[DD <: Dimension[_, _, _, _, _, _, _]] extends ScalarQuantitiesOperation[D x DD] {
    val constant: Quantity[DD, N]
    val initialDimension: Int
    lazy val unit = self.unit * constant.unit
    override def coordinate(i: Int, j: Int): Double = {
      var sum = 0.0
      for (i <- 0 until initialDimension) {
        sum += self.coordinate(i, j) * constant.coordinate(i)
      }
      sum
    }
  }

  trait VectorDivision[DD <: Dimension[_, _, _, _, _, _, _]] extends VectorQuantitiesOperation[D / DD] {
    val other: QuantitiesLike[DD, Scalar]
    lazy val unit = self.unit / other.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) / other.coordinate(0, j)
    }
  }

  trait ConstantDivision[DD <: Dimension[_, _, _, _, _, _, _]] extends VectorQuantitiesOperation[D / DD] {
    val constant: ScalarQuantity[DD]
    lazy val unit = self.unit / constant.unit
    override def coordinate(i: Int, j: Int): Double = {
      self.coordinate(i, j) / constant.coordinate(0)
    }
  }

  trait ConversionOperation extends VectorQuantitiesOperation[D] {
    val thatUnit = self.unit
    override def coordinate(i: Int, j: Int): Double = {
      unit.converter.inverse(self.coordinate(i, j), thatUnit.converter)
    }
  }

  trait DiffOperation extends VectorQuantitiesOperation[D] {
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
      if (j >= 0 && j + from < until) self.coordinate(i, j + from)
      else throw new IndexOutOfBoundsException(j.toString)
    }
  }

  trait Filtered extends super.Filtered with Transformed[D] {
    val unit = self.unit
    val dimension = self.dimension
    override def coordinate(i: Int, j: Int): Double = self.coordinate(i, index(j))
  }

  trait TakenWhile extends super.TakenWhile with Transformed[D] {
    val unit = self.unit
    val dimension = self.dimension
    override def coordinate(i: Int, j: Int): Double =
      if (j < len) self.coordinate(i, j)
      else throw new IndexOutOfBoundsException(j.toString)
  }

  trait DroppedWhile extends super.DroppedWhile with Transformed[D] {
    val unit = self.unit
    val dimension = self.dimension
    override def coordinate(i: Int, j: Int): Double =
      if (j >= 0) self.coordinate(i, j + start)
      else throw new IndexOutOfBoundsException(j.toString)
  }

  trait Reversed extends super.Reversed with Transformed[D] {
    val unit = self.unit
    val dimension = self.dimension
    override def coordinate(i: Int, j: Int): Double =
      self.coordinate(i, self.length - 1 - j)
  }

}

object VectorQuantitiesView {
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
}