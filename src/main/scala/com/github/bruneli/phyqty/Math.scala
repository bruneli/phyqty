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

import Dimension.DimensionLess

/**
  * @author bruneli
  */
object Math {

  def ceil[D <: Dimension[_, _, _, _, _, _, _]](quantity: Quantity[D]): Quantity[D] = {
    applyFunction(quantity, math.ceil)
  }

  def ceil[D <: Dimension[_, _, _, _, _, _, _]](quantities: QuantitiesLike[D]): QuantitiesLike[D] = {
    quantities.mapMagnitudes(math.ceil)
  }

  def floor[D <: Dimension[_, _, _, _, _, _, _]](quantity: Quantity[D]): Quantity[D] = {
    applyFunction(quantity, math.floor)
  }

  def floor[D <: Dimension[_, _, _, _, _, _, _]](quantities: QuantitiesLike[D]): QuantitiesLike[D] = {
    quantities.mapMagnitudes(math.floor)
  }

  def abs[D <: Dimension[_, _, _, _, _, _, _]](quantity: Quantity[D]): Quantity[D] = {
    applyFunction(quantity, math.abs)
  }

  def abs[D <: Dimension[_, _, _, _, _, _, _]](quantities: QuantitiesLike[D]): QuantitiesLike[D] = {
    quantities.mapMagnitudes(math.abs)
  }

  def sqrt(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.sqrt)
  }

  def sqrt(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.sqrt)
  }

  def cbrt(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.cbrt)
  }

  def cbrt(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.cbrt)
  }

  def exp(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.exp)
  }

  def exp(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.exp)
  }

  def log(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.log)
  }

  def log(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.log)
  }

  def log10(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.log10)
  }

  def log10(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.log10)
  }

  def sin(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.sin)
  }

  def sin(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.sin)
  }

  def cos(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.cos)
  }

  def cos(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.cos)
  }

  def tan(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.tan)
  }

  def tan(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.tan)
  }

  def asin(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.asin)
  }

  def asin(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.asin)
  }

  def acos(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.acos)
  }

  def acos(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.acos)
  }

  def atan(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.atan)
  }

  def atan(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.atan)
  }

  def sinh(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.sinh)
  }

  def sinh(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.sinh)
  }

  def cosh(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.cosh)
  }

  def cosh(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.cosh)
  }

  def tanh(quantity: Quantity[DimensionLess]): Quantity[DimensionLess] = {
    applyFunction(quantity, math.tanh)
  }

  def tanh(quantities: QuantitiesLike[DimensionLess]): QuantitiesLike[DimensionLess] = {
    quantities.mapMagnitudes(math.tanh)
  }

  def max[D <: Dimension[_, _, _, _, _, _, _]](q1: Quantity[D], q2: Quantity[D]): Quantity[D] = {
    val q3 = q2.in(q1.unit)
    q1.copy(magnitude = math.max(q1.magnitude, q3.magnitude))
  }

  def max[D <: Dimension[_, _, _, _, _, _, _]](q1: QuantitiesLike[D], q2: QuantitiesLike[D]): QuantitiesLike[D] = {
    applyArity2Function(q1, q2)(math.max)
  }

  def min[D <: Dimension[_, _, _, _, _, _, _]](q1: Quantity[D], q2: Quantity[D]): Quantity[D] = {
    val q3 = q2.in(q1.unit)
    q1.copy(magnitude = math.min(q1.magnitude, q3.magnitude))
  }

  def min[D <: Dimension[_, _, _, _, _, _, _]](q1: QuantitiesLike[D], q2: QuantitiesLike[D]): QuantitiesLike[D] = {
    applyArity2Function(q1, q2)(math.min)
  }

  def applyFunction[D <: Dimension[_, _, _, _, _, _, _]](quantity: Quantity[D], function: Double => Double): Quantity[D] = {
    Quantity(function(quantity.magnitude), quantity.unit)
  }

  def applyArity2Function[D <: Dimension[_, _, _, _, _, _, _]](q1: QuantitiesLike[D], q2: QuantitiesLike[D])(
    function: (Double, Double) => Double): Quantities[D] = {
    if (q1.length != q2.length) {
      throw new Quantities.QuantitiesDimensionException
    } else {
      val q3 = q2.in(q1.unit)
      val magnitudes = new Array[Double](q1.length)
      for (idx <- magnitudes.indices) {
        magnitudes(idx) = function(q1.magnitude(idx), q3.magnitude(idx))
      }
      Quantities(magnitudes, q1.unit)
    }
  }

}
