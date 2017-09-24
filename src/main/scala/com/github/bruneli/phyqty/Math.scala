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

  def ceil[D <: Dimension[_, _, _, _, _, _, _]](quantity: ScalarQuantity[D]): ScalarQuantity[D] = {
    applyFunction(quantity, math.ceil)
  }

  def ceil[D <: Dimension[_, _, _, _, _, _, _]](quantities: QuantitiesLike[D, Scalar]): QuantitiesLike[D, Scalar] = {
    quantities.mapCoordinates(math.ceil)
  }

  def floor[D <: Dimension[_, _, _, _, _, _, _]](quantity: ScalarQuantity[D]): ScalarQuantity[D] = {
    applyFunction(quantity, math.floor)
  }

  def floor[D <: Dimension[_, _, _, _, _, _, _]](quantities: QuantitiesLike[D, Scalar]): QuantitiesLike[D, Scalar] = {
    quantities.mapCoordinates(math.floor)
  }

  def abs[D <: Dimension[_, _, _, _, _, _, _]](quantity: ScalarQuantity[D]): ScalarQuantity[D] = {
    applyFunction(quantity, math.abs)
  }

  def abs[D <: Dimension[_, _, _, _, _, _, _]](quantities: QuantitiesLike[D, Scalar]): QuantitiesLike[D, Scalar] = {
    quantities.mapCoordinates(math.abs)
  }

  def sqrt(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.sqrt)
  }

  def sqrt(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.sqrt)
  }

  def cbrt(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.cbrt)
  }

  def cbrt(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.cbrt)
  }

  def exp(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.exp)
  }

  def exp(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.exp)
  }

  def log(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.log)
  }

  def log(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.log)
  }

  def log10(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.log10)
  }

  def log10(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.log10)
  }

  def sin(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.sin)
  }

  def sin(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.sin)
  }

  def cos(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.cos)
  }

  def cos(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.cos)
  }

  def tan(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.tan)
  }

  def tan(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.tan)
  }

  def asin(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.asin)
  }

  def asin(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.asin)
  }

  def acos(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.acos)
  }

  def acos(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.acos)
  }

  def atan(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.atan)
  }

  def atan(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.atan)
  }

  def sinh(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.sinh)
  }

  def sinh(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.sinh)
  }

  def cosh(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.cosh)
  }

  def cosh(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.cosh)
  }

  def tanh(quantity: ScalarQuantity[DimensionLess]): ScalarQuantity[DimensionLess] = {
    applyFunction(quantity, math.tanh)
  }

  def tanh(quantities: QuantitiesLike[DimensionLess, Scalar]): QuantitiesLike[DimensionLess, Scalar] = {
    quantities.mapCoordinates(math.tanh)
  }

  def max[D <: Dimension[_, _, _, _, _, _, _]](q1: ScalarQuantity[D], q2: ScalarQuantity[D]): ScalarQuantity[D] = {
    val q3 = q2.in(q1.unit)
    q1.copy(magnitude = math.max(q1.magnitude, q3.magnitude))
  }

  def max[D <: Dimension[_, _, _, _, _, _, _]](q1: QuantitiesLike[D, Scalar], q2: QuantitiesLike[D, Scalar]): QuantitiesLike[D, Scalar] = {
    applyArity2Function(q1, q2)(math.max)
  }

  def min[D <: Dimension[_, _, _, _, _, _, _]](q1: ScalarQuantity[D], q2: ScalarQuantity[D]): ScalarQuantity[D] = {
    val q3 = q2.in(q1.unit)
    q1.copy(magnitude = math.min(q1.magnitude, q3.magnitude))
  }

  def min[D <: Dimension[_, _, _, _, _, _, _]](q1: QuantitiesLike[D, Scalar], q2: QuantitiesLike[D, Scalar]): QuantitiesLike[D, Scalar] = {
    applyArity2Function(q1, q2)(math.min)
  }

  def applyFunction[D <: Dimension[_, _, _, _, _, _, _]](quantity: ScalarQuantity[D], function: Double => Double): ScalarQuantity[D] = {
    ScalarQuantity(function(quantity.magnitude), quantity.unit)
  }

  def applyArity2Function[D <: Dimension[_, _, _, _, _, _, _]](q1: QuantitiesLike[D, Scalar], q2: QuantitiesLike[D, Scalar])(
    function: (Double, Double) => Double): ScalarQuantities[D] = {
    if (q1.length != q2.length) {
      throw new Quantities.QuantitiesDimensionException
    } else {
      val q3 = q2.in(q1.unit)
      val magnitudes = new Array[Double](q1.length)
      for (idx <- magnitudes.indices) {
        magnitudes(idx) = function(q1.magnitude(idx), q3.magnitude(idx))
      }
      ScalarQuantities(magnitudes, q1.unit)
    }
  }

}
