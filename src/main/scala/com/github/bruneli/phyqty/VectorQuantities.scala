package com.github.bruneli.phyqty

import scala.collection.generic.CanBuildFrom
import scala.collection.{IndexedSeqLike, mutable}

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

/**
  * @author bruneli
  */
case class VectorQuantities[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType](coordinates: Array[Double], unit: PhyUnit[D], dimension: Int)
  extends IndexedSeq[VectorQuantity[D, N]]
    with IndexedSeqLike[VectorQuantity[D, N], VectorQuantities[D, N]]
    with Quantities[D, N] {
  self =>

  type Q[DD <: Dimension[_, _, _, _, _, _, _]] = VectorQuantities[DD, N]

  lazy val length: Int = coordinates.length / dimension

  def apply(j: Int): VectorQuantity[D, N] = {
    val values = new Array[Double](dimension)
    for (i <- 0 until dimension) {
      values(i) = coordinate(i, j)
    }
    VectorQuantity(values, unit)
  }

  override def magnitude(j: Int): Double = {
    var sum = 0.0
    for (i <- 0 until dimension) {
      sum += coordinate(i, j) * coordinate(i, j)
    }
    math.sqrt(sum)
  }

  override def newBuilder: mutable.Builder[VectorQuantity[D, N], VectorQuantities[D, N]] = {
    VectorQuantities.newBuilder
  }

  override def sum[B >: VectorQuantity[D, N]](implicit num: Numeric[B]): B = {
    super[Quantities].sum.asInstanceOf[B]
  }

  override def view = new VectorQuantitiesView[D, N, VectorQuantities[D, N]] {
    val unit = self.unit
    val dimension = self.dimension
    protected lazy val underlying = self.repr

    override def iterator = self.iterator

    override def length = self.length

    override def apply(j: Int) = self.apply(j)

    override def coordinate(i: Int, j: Int): Double = self.coordinate(i, j)
  }

  override def view(from: Int, until: Int) = view.slice(from, until)

  override def diff(offset: Int): VectorQuantities[D, N] = {
    view.diff(offset).force
  }

  override def force: VectorQuantities[D, N] = this

  def coordinate(row: Int, col: Int): Double = {
    coordinates(row * length + col)
  }

  protected def buildQuantity[DD <: Dimension[_, _, _, _, _, _, _]](coordinates: Array[Double], unit: PhyUnit[DD]): VectorQuantity[DD, N] = {
    VectorQuantity(coordinates, unit)
  }

  protected def buildQuantities[DD <: Dimension[_, _, _, _, _, _, _]](coordinates: Array[Double], unit: PhyUnit[DD]): VectorQuantities[DD, N] = {
    VectorQuantities(coordinates, unit, dimension)
  }

}

object VectorQuantities {

  def fill[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType](n: Int)(elem: => VectorQuantity[D, N]): VectorQuantities[D, N] = {
    VectorQuantities(elem.coordinates.flatMap(value => Array.fill(n)(value)), elem.unit, elem.length)
  }

  def iterate[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType](start: VectorQuantity[D, N], len: Int)(
    f: VectorQuantity[D, N] => VectorQuantity[D, N]): VectorQuantities[D, N] = {
    fromVector(Vector.iterate(start, len)(f))
  }

  def apply[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType](quantities: VectorQuantity[D, N]*): VectorQuantities[D, N] = {
    fromVector(quantities.toVector)
  }

  def apply[D <: Dimension[_, _, _, _, _, _, _]](x: ScalarQuantities[D],
                                                 y: ScalarQuantities[D]): VectorQuantities[D, Vector2D] = {
    require(x.length == y.length, "x and y should be of equal length")
    val coordinates = x.magnitudes ++ y.in(x.unit).asInstanceOf[ScalarQuantities[D]].magnitudes
    new VectorQuantities[D, Vector2D](coordinates, x.unit, 2)
  }

  def apply[D <: Dimension[_, _, _, _, _, _, _]](x: ScalarQuantities[D],
                                                 y: ScalarQuantities[D],
                                                 z: ScalarQuantities[D]): VectorQuantities[D, Vector3D] = {
    require(x.length == y.length, "x and y should be of equal length")
    require(x.length == z.length, "x and z should be of equal length")
    val coordinates: Array[Double] =
      x.magnitudes ++
      y.in(x.unit).asInstanceOf[ScalarQuantities[D]].magnitudes ++
      z.in(x.unit).asInstanceOf[ScalarQuantities[D]].magnitudes
    new VectorQuantities[D, Vector3D](coordinates, x.unit, 3)
  }

  def newBuilder[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType]: mutable.Builder[VectorQuantity[D, N], VectorQuantities[D, N]] = {
    Vector.newBuilder[VectorQuantity[D, N]].mapResult(VectorQuantities.fromVector)
  }

  implicit def canBuildFrom[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType]: CanBuildFrom[VectorQuantities[_, _], VectorQuantity[D, N], VectorQuantities[D, N]] = {
    new CanBuildFrom[VectorQuantities[_, _], VectorQuantity[D, N], VectorQuantities[D, N]] {
      def apply(from: VectorQuantities[_, _]) = newBuilder[D, N]

      def apply() = newBuilder[D, N]
    }
  }

  private def fromVector[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType](quantities: Vector[VectorQuantity[D, N]]): VectorQuantities[D, N] = {
    if (quantities.isEmpty) {
      VectorQuantities(Array.emptyDoubleArray, PhyUnit[D]("a.u."), 0)
    } else {
      val unit = quantities.head.unit
      val indices = quantities.head.coordinates.indices.toArray
      VectorQuantities(indices.flatMap(i => quantities.map(_.in(unit).coordinate(i)).toArray[Double]), unit, quantities.head.length)
    }
  }

}
