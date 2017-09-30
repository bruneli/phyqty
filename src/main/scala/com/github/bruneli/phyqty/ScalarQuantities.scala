package com.github.bruneli.phyqty

import scala.collection.generic.CanBuildFrom
import scala.collection.{Iterator, IndexedSeqLike, mutable}

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
case class ScalarQuantities[D <: Dimension[_, _, _, _, _, _, _]](magnitudes: Array[Double], unit: PhyUnit[D])
  extends IndexedSeq[ScalarQuantity[D]]
    with IndexedSeqLike[ScalarQuantity[D], ScalarQuantities[D]]
    with Quantities[D, Scalar] {
  self =>

  type Q[DD <: Dimension[_, _, _, _, _, _, _]] = ScalarQuantities[DD]

  lazy val dimension: Int = 1

  lazy val length: Int = magnitudes.length

  def apply(idx: Int): ScalarQuantity[D] = {
    ScalarQuantity(magnitudes(idx), unit)
  }

  override def magnitude(idx: Int) = magnitudes(idx)

  override def newBuilder: mutable.Builder[ScalarQuantity[D], ScalarQuantities[D]] = {
    ScalarQuantities.newBuilder
  }

  override def sum[B >: ScalarQuantity[D]](implicit num: Numeric[B]): B = {
    super[Quantities].sum.asInstanceOf[B]
  }

  override def view = new ScalarQuantitiesView[D, ScalarQuantities[D]] {
    val unit = self.unit
    val dimension = self.dimension
    protected lazy val underlying = self.repr

    override def iterator = self.iterator

    override def length = self.length

    override def apply(j: Int) = self.apply(j)

    override def coordinate(i: Int, j: Int): Double = self.coordinate(i, j)
  }

  override def view(from: Int, until: Int) = view.slice(from, until)

  override def diff(offset: Int): Quantities[D, Scalar] = {
    view.diff(offset).force
  }

  override def force: ScalarQuantities[D] = this

  def coordinate(row: Int, col: Int): Double = {
    magnitudes(col)
  }

  def dropna: ScalarQuantities[D] = {
    this.filterNot(_.magnitude.isNaN)
  }

  protected def buildQuantity[DD <: Dimension[_, _, _, _, _, _, _]](coordinates: Array[Double], unit: PhyUnit[DD]): ScalarQuantity[DD] = {
    ScalarQuantity(coordinates(0), unit)
  }

  protected def buildQuantities[DD <: Dimension[_, _, _, _, _, _, _]](coordinates: Array[Double], unit: PhyUnit[DD]): ScalarQuantities[DD] = {
    ScalarQuantities(coordinates, unit)
  }

}

object ScalarQuantities {

  def fill[D <: Dimension[_, _, _, _, _, _, _]](n: Int)(elem: => ScalarQuantity[D]): ScalarQuantities[D] = {
    ScalarQuantities(Array.fill(n)(elem.magnitude), elem.unit)
  }

  def iterate[D <: Dimension[_, _, _, _, _, _, _]](start: ScalarQuantity[D], len: Int)(
    f: ScalarQuantity[D] => ScalarQuantity[D]): ScalarQuantities[D] = {
    val magnitudes = Array.iterate(start.magnitude, len)(previous => f(ScalarQuantity(previous, start.unit)).magnitude)
    ScalarQuantities(magnitudes, start.unit)
  }

  def fromVector[D <: Dimension[_, _, _, _, _, _, _]](quantities: Vector[ScalarQuantity[D]]): ScalarQuantities[D] = {
    if (quantities.isEmpty) {
      ScalarQuantities(Array.emptyDoubleArray, PhyUnit[D]("a.u."))
    } else {
      val unit = quantities.head.unit
      ScalarQuantities(quantities.map(_.in(unit).magnitude).toArray, unit)
    }
  }

  def apply[D <: Dimension[_, _, _, _, _, _, _]](quantities: ScalarQuantity[D]*): ScalarQuantities[D] = {
    fromVector(quantities.toVector)
  }

  def newBuilder[D <: Dimension[_, _, _, _, _, _, _]]: mutable.Builder[ScalarQuantity[D], ScalarQuantities[D]] = {
    Vector.newBuilder[ScalarQuantity[D]].mapResult(ScalarQuantities.fromVector)
  }

  implicit def canBuildFrom[D <: Dimension[_, _, _, _, _, _, _]]: CanBuildFrom[ScalarQuantities[_], ScalarQuantity[D], ScalarQuantities[D]] = {
    new CanBuildFrom[ScalarQuantities[_], ScalarQuantity[D], ScalarQuantities[D]] {
      def apply(from: ScalarQuantities[_]) = newBuilder[D]

      def apply() = newBuilder[D]
    }
  }

}
