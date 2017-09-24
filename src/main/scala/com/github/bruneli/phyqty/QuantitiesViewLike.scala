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

import scala.collection.generic.SliceInterval
import scala.collection.{IndexedSeq, IndexedSeqLike, SeqView, SeqViewLike}

/**
  * @author bruneli
  */
//trait QuantitiesViewLike[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType] extends IndexedSeq[Quantity[D, N]]
//  with IndexedSeqLike[Quantity[D, N], QuantitiesViewLike[D, N]]
//  with SeqViewLike[Quantity[D, N], Quantities[D, N], QuantitiesViewLike[D, N]]
//  with QuantitiesLike[D, N]
//  with SeqView[Quantity[D, N], Quantities[D, N]] {
//  self =>
//
//  private[this] type This = QuantitiesViewLike[D, N]
//
//  trait Transformed[B <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesViewLike[B, N] with super.Transformed[Quantity[D, N]] {
//    override def toString = viewToString
//  }
//
//  /** Explicit instantiation of the `Transformed` trait to reduce class file size in subclasses. */
//  private abstract class AbstractTransformed[B <: Dimension[_, _, _, _, _, _, _]] extends Transformed[B]
//
//  override def magnitude(idx: Int): Double = coordinate(idx, 0)
//
//  override def mapCoordinates(g: Double => Double): QuantitiesViewLike[D, ScalarQuantities[D]] = {
//    new MappingOperation {
//      val f = g
//    }
//  }
//
//  override def + (that: QuantitiesLike[D, Scalar]): QuantitiesViewLike[D, ScalarQuantities[D]] = {
//    new VectorAddition {
//      val other = that
//    }
//  }
//
//  override def + (that: Quantity[D, Scalar]): QuantitiesViewLike[D, ScalarQuantities[D]] = {
//    new ConstantAddition {
//      val constant = that
//    }
//  }
//
//  override def - (that: QuantitiesLike[D, Scalar]): QuantitiesViewLike[D, ScalarQuantities[D]] = {
//    new VectorSubtraction {
//      val other = that
//    }
//  }
//
//  override def - (that: Quantity[D, Scalar]): QuantitiesViewLike[D, ScalarQuantities[D]] = {
//    new ConstantSubtraction {
//      val constant = that
//    }
//  }
//
//  override def unary_-(): QuantitiesViewLike[D, ScalarQuantities[D]] = {
//    new MappingOperation {
//      override val f = (x: Double) => -x
//    }
//  }
//
//  override def * [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD, Scalar]): QuantitiesViewLike[D x DD, ScalarQuantities[D x DD]] = {
//    new VectorMultiplication[DD] {
//      val other = that
//    }
//  }
//
//  override def * [DD <: Dimension[_, _, _, _, _, _, _]](that: ScalarQuantity[DD]): QuantitiesViewLike[D x DD, ScalarQuantities[D x DD]] = {
//    new ConstantMultiplication[DD] {
//      val constant = that
//    }
//  }
//
//  override def * (scalar: Double): QuantitiesViewLike[D, ScalarQuantities[D]] = {
//    new MappingOperation {
//      override val f = (x: Double) => x * scalar
//    }
//  }
//
//  override def / [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD, ScalarQuantity[DD]]): QuantitiesViewLike[D / DD, ScalarQuantities[D / DD]] = {
//    new VectorDivision[DD] {
//      val other = that
//    }
//  }
//
//  override def / [DD <: Dimension[_, _, _, _, _, _, _]](that: ScalarQuantity[DD]): QuantitiesViewLike[D / DD, ScalarQuantities[D / DD]] = {
//    new ConstantDivision[DD] {
//      val constant = that
//    }
//  }
//
//  override def / (scalar: Double): QuantitiesViewLike[D, ScalarQuantities[D]] = {
//    new MappingOperation {
//      override val f = (x: Double) => x / scalar
//    }
//  }
//
//  override def in(anotherUnit: PhyUnit[D]): QuantitiesViewLike[D, ScalarQuantities[D]] = {
//    new ConversionOperation {
//      override val thatUnit = anotherUnit
//    }
//  }
//
//  override def diff(idx: Int): QuantitiesViewLike[D, ScalarQuantities[D]] = {
//    new DiffOperation {
//      override val offset = idx
//    }
//  }
//
//  override protected def buildQuantity[DD <: Dimension[_, _, _, _, _, _, _]](coordinates: Array[Double],
//                                                                             unit: PhyUnit[DD]): Quantity[DD, N] = {
//    N match
//      case Scalar =>
//    if (dimension == 1) {
//      ScalarQuantity(coordinates(0), unit)
//    }
//  }
//
//  protected override def newFiltered(p: Quantity[D, N] => Boolean): Transformed[D] = {
//    new { val pred = p } with AbstractTransformed[D] with Filtered
//  }
//  protected def newSliced(from: Int, until: Int): Transformed[D] = {
//    new { val endpoints = SliceInterval(from, until) } with AbstractTransformed[D] with Sliced
//  }
//  protected override def newDroppedWhile(p: Quantity[D, N] => Boolean): Transformed[D] = {
//    new { val pred = p } with AbstractTransformed[D] with DroppedWhile
//  }
//  protected override def newTakenWhile(p: Quantity[D, N] => Boolean): Transformed[D] = {
//    new { val pred = p } with AbstractTransformed[D] with TakenWhile
//  }
//  protected override def newReversed: Transformed[D] = new AbstractTransformed[D] with Reversed
//
//  override def init: This = newSliced(0, self.length - 1)
//  override def drop(n: Int): This = newSliced(n, self.length)
//  override def take(n: Int): This = newSliced(0, n min self.length)
//  override def slice(from: Int, until: Int): This = newSliced(from, until min self.length)
//  override def dropWhile(p: Quantity[D, N] => Boolean): This = newDroppedWhile(p)
//  override def takeWhile(p: Quantity[D, N] => Boolean): This = newTakenWhile(p)
//  override def span(p: Quantity[D, N] => Boolean): (This, This) = (newTakenWhile(p), newDroppedWhile(p))
//  override def splitAt(n: Int): (This, This) = (take(n), drop(n)) // !!!
//  override def reverse: This = newReversed
//  override def tail: QuantitiesViewLike[D, N] = if (isEmpty) super.tail else slice(1, length)
//  override def stringPrefix = "ScalarQuantitiesView"
//
//  trait QuantitiesOperation[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesViewLike[DD, ScalarQuantities[DD]] {
//    protected lazy val underlying = ???
//    override def length = self.length
//    val dimension = self.dimension
//    override def apply(idx: Int) = ScalarQuantity[DD](magnitude(idx), unit)
//  }
//
//  trait MappingOperation extends QuantitiesOperation[D] {
//    val f: Double => Double
//    val unit = self.unit
//    override def coordinate(i: Int, j: Int): Double = {
//      f(self.coordinate(i, j))
//    }
//  }
//
//  trait VectorAddition extends QuantitiesOperation[D] {
//    val other: QuantitiesLike[D, Scalar]
//    val unit = self.unit
//    override def coordinate(i: Int, j: Int): Double = {
//      self.coordinate(i, j) + converter(other.unit)(other.coordinate(i, j))
//    }
//  }
//
//  trait ConstantAddition extends QuantitiesOperation[D] {
//    val constant: ScalarQuantity[D]
//    val unit = self.unit
//    override def coordinate(i: Int, j: Int): Double = {
//      self.coordinate(i, j) + converter(constant.unit)(constant.coordinate(j))
//    }
//  }
//
//  trait VectorSubtraction extends QuantitiesOperation[D] {
//    val other: QuantitiesLike[D, Scalar]
//    val unit = self.unit
//    override def coordinate(i: Int, j: Int): Double = {
//      self.coordinate(i, j) - converter(other.unit)(other.coordinate(i, j))
//    }
//  }
//
//  trait ConstantSubtraction extends QuantitiesOperation[D] {
//    val constant: ScalarQuantity[D]
//    val unit = self.unit
//    override def coordinate(i: Int, j: Int): Double = {
//      self.coordinate(i, j) - converter(constant.unit)(constant.coordinate(j))
//    }
//  }
//
//  trait VectorMultiplication[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesOperation[D x DD] {
//    val other: QuantitiesLike[DD, Scalar]
//    lazy val unit = self.unit * other.unit
//    override def coordinate(i: Int, j: Int): Double = {
//      self.coordinate(i, j) * other.coordinate(i, j)
//    }
//  }
//
//  trait ConstantMultiplication[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesOperation[D x DD] {
//    val constant: ScalarQuantity[DD]
//    lazy val unit = self.unit * constant.unit
//    override def coordinate(i: Int, j: Int): Double = {
//      self.coordinate(i, j) * constant.coordinate(j)
//    }
//  }
//
//  trait VectorDivision[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesOperation[D / DD] {
//    val other: QuantitiesLike[DD, ScalarQuantity[DD]]
//    lazy val unit = self.unit / other.unit
//    override def coordinate(i: Int, j: Int): Double = {
//      self.coordinate(i, j) / other.coordinate(i, 0)
//    }
//  }
//
//  trait ConstantDivision[DD <: Dimension[_, _, _, _, _, _, _]] extends QuantitiesOperation[D / DD] {
//    val constant: ScalarQuantity[DD]
//    lazy val unit = self.unit / constant.unit
//    override def coordinate(i: Int, j: Int): Double = {
//      self.coordinate(i, j) / constant.coordinate(0)
//    }
//  }
//
//  trait ConversionOperation extends QuantitiesOperation[D] {
//    val unit = self.unit
//    val thatUnit: PhyUnit[D]
//    override def coordinate(i: Int, j: Int): Double = {
//      thatUnit.converter.inverse(self.coordinate(i, j), unit.converter)
//    }
//  }
//
//  trait DiffOperation extends QuantitiesOperation[D] {
//    val unit = self.unit
//    val offset: Int
//    override def coordinate(i: Int, j: Int): Double = {
//      if (offset > 0 && i < offset || offset < 0 && i > length - offset) {
//        Double.NaN
//      } else {
//        self.coordinate(i, j) - self.coordinate(i - offset, j)
//      }
//    }
//  }
//
//  trait Sliced extends super.Sliced with Transformed[D] {
//    override def length = endpoints.width
//    val unit = self.unit
//    val dimension = self.dimension
//    override def coordinate(i: Int, j: Int): Double = {
//      if (i >= 0 && i + from < until) self.coordinate(i + from, j)
//      else throw new IndexOutOfBoundsException(i.toString)
//    }
//  }
//
//  trait Filtered extends super.Filtered with Transformed[D] {
//    val unit = self.unit
//    val dimension = self.dimension
//    override def coordinate(i: Int, j: Int): Double = self.coordinate(index(i), j)
//  }
//
//  trait TakenWhile extends super.TakenWhile with Transformed[D] {
//    val unit = self.unit
//    val dimension = self.dimension
//    override def coordinate(i: Int, j: Int): Double =
//      if (i < len) self.coordinate(i, j)
//      else throw new IndexOutOfBoundsException(i.toString)
//  }
//
//  trait DroppedWhile extends super.DroppedWhile with Transformed[D] {
//    val unit = self.unit
//    val dimension = self.dimension
//    override def coordinate(i: Int, j: Int): Double =
//      if (i >= 0) self.coordinate(i + start, j)
//      else throw new IndexOutOfBoundsException(i.toString)
//  }
//
//  trait Reversed extends super.Reversed with Transformed[D] {
//    val unit = self.unit
//    val dimension = self.dimension
//    override def coordinate(i: Int, j: Int): Double =
//      self.coordinate(self.length - 1 - i, j)
//  }
//
//}
//
