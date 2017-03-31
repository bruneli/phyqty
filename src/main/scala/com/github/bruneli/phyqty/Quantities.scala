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

import Quantities.QuantitiesDimensionException

import scala.collection.generic.CanBuildFrom
import scala.collection.{IndexedSeqLike, mutable}

import Dimension.{x, /}

/**
  * @author bruneli
  */
trait Quantities[D <: Dimension[_, _, _, _, _, _, _]] extends IndexedSeq[Quantity[D]]
  with IndexedSeqLike[Quantity[D], Quantities[D]]
  with QuantitiesLike[D] {
  self =>

  val magnitudes: Array[Double]

  override def length: Int = magnitudes.length

  override def newBuilder: mutable.Builder[Quantity[D], Quantities[D]] = {
    Quantities.newBuilder
  }

  override def magnitude(idx: Int) = magnitudes(idx)

  override def mapMagnitudes(f: Double => Double): Quantities[D] = {
    val mappedMagnitudes = new Array[Double](length)
    for (idx <- magnitudes.indices) {
      mappedMagnitudes(idx) = f(magnitudes(idx))
    }
    Quantities(mappedMagnitudes, unit)
  }

  override def apply(idx: Int): Quantity[D] = Quantity(magnitude(idx), unit)

  override def view = new QuantitiesView[D, Quantities[D]] {
    val unit = self.unit
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
    override def length = self.length
    override def magnitude(idx: Int) = self.magnitudes(idx)
    override def apply(idx: Int) = self.apply(idx)
  }

  override def view(from: Int, until: Int) = view.slice(from, until)

  override def force: Quantities[D] = this

  override def sum[B >: Quantity[D]](implicit num: Numeric[B]): B = {
    super[QuantitiesLike].sum
  }

  override def + (that: QuantitiesLike[D]): Quantities[D] = {
    if (this.length == that.length) {
      val uniformize: Double => Double = converter(that.unit)
      val diff = new Array[Double](this.length)
      for (idx <- magnitudes.indices) {
        diff(idx) = this.magnitudes(idx) + uniformize(that.magnitude(idx))
      }
      Quantities(diff, unit)
    } else {
      throw new QuantitiesDimensionException
    }
  }

  override def + (that: Quantity[D]): Quantities[D] = {
    val constant = converter(that.unit)(that.magnitude)
    Quantities(magnitudes.map(_ + constant), unit)
  }

  override def - (that: QuantitiesLike[D]): Quantities[D] = {
    if (this.length == that.length) {
      val uniformize: Double => Double = converter(that.unit)
      val diff = new Array[Double](this.length)
      for (idx <- magnitudes.indices) {
        diff(idx) = this.magnitudes(idx) - uniformize(that.magnitude(idx))
      }
      Quantities(diff, unit)
    } else {
      throw new QuantitiesDimensionException
    }
  }

  override def - (that: Quantity[D]): Quantities[D] = {
    val constant = converter(that.unit)(that.magnitude)
    Quantities(magnitudes.map(_ - constant), unit)
  }

  override def unary_-(): Quantities[D] = {
    Quantities(magnitudes.map(x => -x), unit)
  }

  override def * [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD]): Quantities[D x DD] = {
    if (this.length == that.length) {
      val prod = new Array[Double](this.length)
      for (idx <- magnitudes.indices) {
        prod(idx) = this.magnitudes(idx) * that.magnitude(idx)
      }
      Quantities(prod, this.unit * that.unit)
    } else {
      throw new QuantitiesDimensionException
    }
  }

  override def * [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD]): Quantities[D x DD] = {
    Quantities(magnitudes.map(_ * that.magnitude), this.unit * that.unit)
  }

  override def * (scalar: Double): Quantities[D] = {
    Quantities(magnitudes.map(_ * scalar), unit)
  }

  override def / [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD]): Quantities[D / DD] = {
    if (this.length == that.length) {
      val ratio = new Array[Double](this.length)
      for (idx <- magnitudes.indices) {
        ratio(idx) = this.magnitudes(idx) / that.magnitude(idx)
      }
      Quantities(ratio, this.unit / that.unit)
    } else {
      throw new QuantitiesDimensionException
    }
  }

  override def / [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD]): Quantities[D / DD] = {
    Quantities(magnitudes.map(_ / that.magnitude), this.unit / that.unit)
  }

  override def / (scalar: Double): Quantities[D] = {
    Quantities(magnitudes.map(_ / scalar), unit)
  }

  override def in(anotherUnit: PhyUnit[D]): Quantities[D] = {
    Quantities(magnitudes.map(x => anotherUnit.converter.inverse(x, unit.converter)), anotherUnit)
  }

}

object Quantities {

  def apply[D <: Dimension[_, _, _, _, _, _, _]](thisMagnitudes: Array[Double], thisUnit: PhyUnit[D]): Quantities[D] = {
    new Quantities[D] {

      lazy val magnitudes = thisMagnitudes

      override val unit: PhyUnit[D] = thisUnit

    }
  }

  def fromVector[D <: Dimension[_, _, _, _, _, _, _]](quantities: Vector[Quantity[D]]): Quantities[D] = {
    if (quantities.isEmpty) {
      Quantities(Array.emptyDoubleArray, PhyUnit[D]("a.u."))
    } else {
      val unit = quantities.head.unit
      Quantities(quantities.map(_.in(unit).magnitude).toArray, unit)
    }
  }

  def apply[D <: Dimension[_, _, _, _, _, _, _]](quantities: Quantity[D]*): Quantities[D] = {
    fromVector(quantities.toVector)
  }

  def newBuilder[D <: Dimension[_, _, _, _, _, _, _]]: mutable.Builder[Quantity[D], Quantities[D]] = {
    Vector.newBuilder[Quantity[D]].mapResult(Quantities.fromVector)
  }

  implicit def canBuildFrom[D <: Dimension[_, _, _, _, _, _, _]]: CanBuildFrom[Quantities[_], Quantity[D], Quantities[D]] = {
    new CanBuildFrom[Quantities[_], Quantity[D], Quantities[D]] {
      def apply(from: Quantities[_]) = newBuilder[D]
      def apply() = newBuilder[D]
    }
  }

  class QuantitiesDimensionException extends RuntimeException("quantities vectors must be equal in size")

}
