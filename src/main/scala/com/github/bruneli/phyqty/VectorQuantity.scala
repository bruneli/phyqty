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

import Dimension.{/, x}

import scala.collection.generic.CanBuildFrom
import scala.collection.{IndexedSeqLike, mutable}

/**
  * @author bruneli
  */
case class VectorQuantity[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType](coordinates: Array[Double], unit: PhyUnit[D])
  extends IndexedSeq[ScalarQuantity[D]]
    with IndexedSeqLike[ScalarQuantity[D], VectorQuantity[D, N]]
    with Quantity[D, N] {

  def magnitude: Double = {
    var sum = 0.0
    for (idx <- coordinates.indices) {
      sum += coordinates(idx) * coordinates(idx)
    }
    math.sqrt(sum)
  }

  def length: Int = {
    coordinates.length
  }

  override def apply(idx: Int): ScalarQuantity[D] = {
    ScalarQuantity(coordinates(idx), unit)
  }

  override def newBuilder: mutable.Builder[ScalarQuantity[D], VectorQuantity[D, N]] = {
    VectorQuantity.newBuilder[D, N]
  }

  def +(that: Quantity[D, N]): VectorQuantity[D, N] = {
    val uniformize: Double => Double = converter(that.unit)
    val sum = new Array[Double](this.length)
    for (idx <- coordinates.indices) {
      sum(idx) = this.coordinate(idx) + uniformize(that.coordinate(idx))
    }
    VectorQuantity(sum, unit)
  }

  def -(that: Quantity[D, N]): VectorQuantity[D, N] = {
    val uniformize: Double => Double = converter(that.unit)
    val diff = new Array[Double](this.length)
    for (idx <- coordinates.indices) {
      diff(idx) = this.coordinate(idx) - uniformize(that.coordinate(idx))
    }
    VectorQuantity(diff, unit)
  }

  def unary_-(): VectorQuantity[D, N] = {
    VectorQuantity(coordinates.map(x => -x), unit)
  }

  def *(scalar: Double): VectorQuantity[D, N] = {
    VectorQuantity(coordinates.map(_ * scalar), unit)
  }

  def *[DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD, N]): ScalarQuantity[D x DD] = {
    var sum = 0.0
    for (idx <- coordinates.indices) {
      sum += this.coordinate(idx) * that.coordinate(idx)
    }
    ScalarQuantity(sum, this.unit * that.unit)
  }

  def *[DD <: Dimension[_, _, _, _, _, _, _]](that: ScalarQuantity[DD]): VectorQuantity[D x DD, N] = {
    VectorQuantity(coordinates.map(_ * that.magnitude), this.unit * that.unit)
  }

  def /(scalar: Double): VectorQuantity[D, N] = {
    VectorQuantity(coordinates.map(_ / scalar), unit)
  }

  def /[DD <: Dimension[_, _, _, _, _, _, _]](that: ScalarQuantity[DD]): VectorQuantity[D / DD, N] = {
    VectorQuantity(coordinates.map(_ / that.magnitude), this.unit / that.unit)
  }

  def in(anotherUnit: PhyUnit[D]): VectorQuantity[D, N] = {
    VectorQuantity(coordinates.map(x => anotherUnit.converter.inverse(x, unit.converter)), anotherUnit)
  }

  def coordinate(i: Int): Double = coordinates(i)

  protected def converter(thatUnit: PhyUnit[D]): Double => Double = {
    if (thatUnit.converter == this.unit.converter) {
      (x: Double) => x
    } else {
      (x: Double) => unit.converter.inverse(x, thatUnit.converter)
    }
  }

  override def toString: String = s"$magnitude $unit"

  override def equals(obj: Any): Boolean = obj match {
    case that: VectorQuantity[D, N] => this.compare(that) == 0
    case _ => false
  }

}

object VectorQuantity {

  def apply[D <: Dimension[_, _, _, _, _, _, _]](x: ScalarQuantity[D],
                                                 y: ScalarQuantity[D]): VectorQuantity[D, Vector2D] = {
    val coordinates = Array(x.magnitude, y.in(x.unit).magnitude)
    new VectorQuantity[D, Vector2D](coordinates, x.unit)
  }

  def apply[D <: Dimension[_, _, _, _, _, _, _]](x: ScalarQuantity[D],
                                                 y: ScalarQuantity[D],
                                                 z: ScalarQuantity[D]): VectorQuantity[D, Vector3D] = {
    val coordinates = Array(x.magnitude, y.in(x.unit).magnitude, z.in(x.unit).magnitude)
    new VectorQuantity[D, Vector3D](coordinates, x.unit)
  }

  def newBuilder[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType]: mutable.Builder[ScalarQuantity[D], VectorQuantity[D, N]] = {
    Vector.newBuilder.mapResult(VectorQuantity.fromVector)
  }

  implicit def canBuildFrom[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType]: CanBuildFrom[VectorQuantity[_, _], ScalarQuantity[D], VectorQuantity[D, N]] = {
    new CanBuildFrom[VectorQuantity[_, _], ScalarQuantity[D], VectorQuantity[D, N]] {
      def apply(from: VectorQuantity[_, _]) = newBuilder[D, N]

      def apply() = newBuilder[D, N]
    }
  }

  class VectorQuantityDimensionException extends RuntimeException("vector quantity must be equal in size")

  private def fromVector[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType](quantities: Vector[ScalarQuantity[D]]): VectorQuantity[D, N] = {
    if (quantities.length == 2 || quantities.length == 3) {
      val unit = quantities.head.unit
      VectorQuantity[D, N](quantities.map(_.in(unit).magnitude).toArray, unit)
    } else {
      throw new VectorQuantityDimensionException
    }
  }

}