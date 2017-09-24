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

import Dimension.{/, x}
import com.github.bruneli.phyqty

/**
  * @author bruneli
  */
trait QuantitiesLike[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType] {

  val unit: PhyUnit[D]

  def magnitude(idx: Int): Double

  def mapCoordinates(f: Double => Double): QuantitiesLike[D, N]

  def length: Int

  def dimension: Int

  def force: Quantities[D, N]

  def + (that: QuantitiesLike[D, N]): QuantitiesLike[D, N]

  def + (that: Quantity[D, N]): QuantitiesLike[D, N]

  def - (that: QuantitiesLike[D, N]): QuantitiesLike[D, N]

  def - (that: Quantity[D, N]): QuantitiesLike[D, N]

  def unary_-(): QuantitiesLike[D, N]

  def * [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD, N]): QuantitiesLike[D x DD, Scalar]

  def * [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD, N]): QuantitiesLike[D x DD, Scalar]

  def * (scalar: Double): QuantitiesLike[D, N]

  def / [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD, Scalar]): QuantitiesLike[D / DD, N]

  def / [DD <: Dimension[_, _, _, _, _, _, _]](that: ScalarQuantity[DD]): QuantitiesLike[D / DD, N]

  def / (scalar: Double): QuantitiesLike[D, N]

  def in(anotherUnit: PhyUnit[D]): QuantitiesLike[D, N]

  def diff(offset: Int): QuantitiesLike[D, N]

  def coordinate(row: Int, col: Int): Double

  protected def buildQuantity[DD <: Dimension[_, _, _, _, _, _, _]](coordinates: Array[Double], unit: PhyUnit[DD]): Quantity[DD, N]

  def inner [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD, N]): Quantity[D x DD, N] = {
    if (this.length != that.length) {
      throw new Quantities.QuantitiesDimensionException
    } else if (this.dimension != that.dimension) {
      throw new VectorQuantity.VectorQuantityDimensionException
    } else {
      val sum = Array.fill(dimension)(0.0)
      for {i <- 0 until dimension
           j <- 0 until length} {
        sum(i) += this.coordinate(i, j) * that.coordinate(i, j)
      }
      buildQuantity(sum, this.unit * that.unit)
    }
  }

  def sum: Quantity[D, N] = {
    val total = Array.fill(dimension)(0.0)
    for {i <- 0 until dimension
         j <- 0 until length} {
      total(i) += coordinate(i, j)
    }
    buildQuantity(total, unit)
  }

  def mean: Quantity[D, N] = {
    if (length == 0) {
      throw new IllegalArgumentException("quantities length is zero, cannot compute the mean")
    } else {
      buildQuantity(meanValues, unit)
    }
  }

  def median: Quantity[D, N] = {
    if (length == 0) {
      throw new IllegalArgumentException("quantities length is zero, cannot compute the median")
    } else {
//      if (length % 2 == 0) {
//        val sortedValues = force.magnitudes.sorted
//        val median = (sortedValues(length / 2 - 1) + sortedValues(length / 2)) * 0.5
//        ScalarQuantity(median, unit)
//      } else {
//        val median = force.magnitudes.sorted.apply(length / 2 + 1)
//        ScalarQuantity(median, unit)
//      }
      buildQuantity(Array.fill(dimension)(0.0), unit)
    }
  }

  def rms: Quantity[D, N] = {
    if (length < 2) {
      throw new IllegalArgumentException("quantities length lower than two, cannot compute the rms")
    } else {
      // TODO use better algo
      val mean = meanValues
      val rms = Array.fill(dimension)(0.0)
      for (i <- 0 until dimension) {
        for (j <- 0 until length) {
          val delta = coordinate(i, j) - mean(i)
          rms(i) += delta * delta
        }
        rms(i) = math.sqrt(rms(i))
      }
      buildQuantity(rms, unit)
    }
  }

  protected def converter(thatUnit: PhyUnit[D]): Double => Double = {
    if (thatUnit.converter == this.unit.converter) {
      (x: Double) => x
    } else {
      (x: Double) => unit.converter.inverse(x, thatUnit.converter)
    }
  }

  private def meanValues: Array[Double] = {
    val mean = Array.fill(dimension)(0.0)
    for (i <- 0 until dimension) {
      for (j <- 0 until length) mean(i) += coordinate(i, j)
      mean(i) /= length
    }
    mean
  }

}
