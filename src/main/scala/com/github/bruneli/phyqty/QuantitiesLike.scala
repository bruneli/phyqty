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

import Dimension.{x, /}

/**
  * @author bruneli
  */
trait QuantitiesLike[D <: Dimension[_, _, _, _, _, _, _]] {

  val unit: PhyUnit[D]

  def magnitude(idx: Int): Double

  def mapMagnitudes(f: Double => Double): QuantitiesLike[D]

  def length: Int

  def force: Quantities[D]

  def + (that: QuantitiesLike[D]): QuantitiesLike[D]

  def + (that: Quantity[D]): QuantitiesLike[D]

  def - (that: QuantitiesLike[D]): QuantitiesLike[D]

  def - (that: Quantity[D]): QuantitiesLike[D]

  def unary_-(): QuantitiesLike[D]

  def * [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD]): QuantitiesLike[D x DD]

  def * [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD]): QuantitiesLike[D x DD]

  def * (scalar: Double): QuantitiesLike[D]

  def / [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD]): QuantitiesLike[D / DD]

  def / [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD]): QuantitiesLike[D / DD]

  def / (scalar: Double): QuantitiesLike[D]

  def in(anotherUnit: PhyUnit[D]): QuantitiesLike[D]

  def diff(offset: Int): QuantitiesLike[D]

  def dropna: QuantitiesLike[D]

  def dot [DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD]): Quantity[D x DD] = {
    if (this.length == that.length) {
      var sum = 0.0
      for (idx <- 0 until length) {
        sum += this.magnitude(idx) * that.magnitude(idx)
      }
      Quantity(sum, this.unit * that.unit)
    } else {
      throw new Quantities.QuantitiesDimensionException
    }
  }

  def sum: Quantity[D] = {
    var total = 0.0
    for (idx <- 0 until length) {
      total += magnitude(idx)
    }
    Quantity(total, unit)
  }

  def mean: Quantity[D] = {
    if (length == 0) {
      throw new IllegalArgumentException("quantities length is zero, cannot compute the mean")
    } else {
      sum / length
    }
  }

  def median: Quantity[D] = {
    if (length == 0) {
      throw new IllegalArgumentException("quantities length is zero, cannot compute the median")
    } else if (length % 2 == 0) {
      val sortedValues = force.magnitudes.sorted
      val median = (sortedValues(length / 2 - 1) + sortedValues(length / 2)) * 0.5
      Quantity(median, unit)
    } else {
      val median = force.magnitudes.sorted.apply(length / 2 + 1)
      Quantity(median, unit)
    }
  }

  def rms: Quantity[D] = {
    if (length < 2) {
      throw new IllegalArgumentException("quantities length lower than two, cannot compute the rms")
    } else {
      // TODO use better algo
      val meanValue = mean.magnitude
      var variance = 0.0
      for (idx <- 0 until length) {
        val delta = magnitude(idx) - meanValue
        variance += delta * delta
      }
      Quantity(math.sqrt(variance), unit)
    }
  }

  protected def converter(thatUnit: PhyUnit[D]): Double => Double = {
    if (thatUnit.converter == this.unit.converter) {
      (x: Double) => x
    } else {
      (x: Double) => unit.converter.inverse(x, thatUnit.converter)
    }
  }

}
