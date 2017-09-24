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

import Dimension.{x, /}

/**
  * @author bruneli
  */
trait Quantity[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType] extends Ordered[Quantity[D, N]] {

  def magnitude: Double

  def unit: PhyUnit[D]

  def + (that: Quantity[D, N]): Quantity[D, N]

  def - (that: Quantity[D, N]): Quantity[D, N]

  def unary_-(): Quantity[D, N]

  def * (scalar: Double): Quantity[D, N]

  def * [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD, N]): ScalarQuantity[D x DD]

  def / (scalar: Double): Quantity[D, N]

  def / [DD <: Dimension[_, _, _, _, _, _, _]](that: ScalarQuantity[DD]): Quantity[D / DD, N]

  def in(anotherUnit: PhyUnit[D]): Quantity[D, N]

  def coordinate(i: Int): Double

  override def compare(that: Quantity[D, N]): Int = {
    this.magnitude.compare(that.in(this.unit).magnitude)
  }

}
