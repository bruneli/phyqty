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

  def abs[D <: Dimension[_, _, _, _, _, _, _]](quantity: Quantity[D]): Quantity[D] = {
    applyFunction(quantity, math.abs)
  }

  def abs[D <: Dimension[_, _, _, _, _, _, _]](quantities: QuantitiesLike[D]): QuantitiesLike[D] = {
    quantities.mapMagnitudes(math.abs)
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

  def applyFunction[D <: Dimension[_, _, _, _, _, _, _]](quantity: Quantity[D], function: Double => Double): Quantity[D] = {
    Quantity(function(quantity.magnitude), quantity.unit)
  }

}
