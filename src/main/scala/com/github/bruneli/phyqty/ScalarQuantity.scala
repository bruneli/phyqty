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
case class ScalarQuantity[D <: Dimension[_, _, _, _, _, _, _]](magnitude: Double, unit: PhyUnit[D]) extends Quantity[D, Scalar] {

  def + (that: Quantity[D, Scalar]): ScalarQuantity[D] = {
    if (that.unit.converter == this.unit.converter) {
      ScalarQuantity(this.magnitude + that.magnitude, unit)
    } else {
      ScalarQuantity(this.magnitude + that.in(unit).magnitude, unit)
    }
  }

  def - (that: Quantity[D, Scalar]): ScalarQuantity[D] = {
    if (that.unit.converter == this.unit.converter) {
      ScalarQuantity(this.magnitude - that.magnitude, unit)
    } else {
      ScalarQuantity(this.magnitude - that.in(unit).magnitude, unit)
    }
  }

  def unary_-(): ScalarQuantity[D] = {
    ScalarQuantity(-magnitude, unit)
  }

  def * (scalar: Double): ScalarQuantity[D] = {
    ScalarQuantity(magnitude * scalar, unit)
  }

  def * [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD, Scalar]): ScalarQuantity[D x DD] = {
    ScalarQuantity(this.magnitude * that.magnitude, this.unit * that.unit)
  }

  def * [DD <: Dimension[_, _, _, _, _, _, _], N <: QuantityType](that: VectorQuantity[DD, N]): VectorQuantity[D x DD, N] = {
    VectorQuantity(that.coordinates.map(_ * that.magnitude), this.unit * that.unit)
  }

  def / (scalar: Double): ScalarQuantity[D] = {
    ScalarQuantity(magnitude / scalar, unit)
  }

  def / [DD <: Dimension[_, _, _, _, _, _, _]](that: ScalarQuantity[DD]): ScalarQuantity[D / DD] = {
    ScalarQuantity(this.magnitude / that.magnitude, this.unit / that.unit)
  }

  def in(anotherUnit: PhyUnit[D]): ScalarQuantity[D] = {
    ScalarQuantity(anotherUnit.converter.inverse(magnitude, unit.converter), anotherUnit)
  }

  def coordinate(i: Int): Double = magnitude

  override def toString: String = s"$magnitude $unit"

  override def equals(obj: Any): Boolean = obj match {
    case that: ScalarQuantity[D] => this.compare(that) == 0
    case _ => false
  }

}

object ScalarQuantity {

  implicit class DoubleToQuantity(val magnitude: Double) extends AnyVal {

    def apply[D <: Dimension[_, _, _, _, _, _, _]](unit: PhyUnit[D]): ScalarQuantity[D] = {
      ScalarQuantity(magnitude, unit)
    }

  }

  implicit class IntToQuantity(val magnitude: Int) extends AnyVal {

    def apply[D <: Dimension[_, _, _, _, _, _, _]](unit: PhyUnit[D]): ScalarQuantity[D] = {
      ScalarQuantity(magnitude, unit)
    }

  }

  implicit class QuantityIsNumeric[D <: Dimension[_, _, _, _, _, _, _]](quantity: ScalarQuantity[D]) extends Numeric[ScalarQuantity[D]] {

    override def plus(x: ScalarQuantity[D], y: ScalarQuantity[D]): ScalarQuantity[D] = x + y

    override def minus(x: ScalarQuantity[D], y: ScalarQuantity[D]): ScalarQuantity[D] = x - y

    override def times(x: ScalarQuantity[D], y: ScalarQuantity[D]): ScalarQuantity[D] = ???

    override def negate(x: ScalarQuantity[D]): ScalarQuantity[D] = -quantity

    override def fromInt(x: Int): ScalarQuantity[D] = ???

    override def toInt(x: ScalarQuantity[D]): Int = quantity.magnitude.toInt

    override def toLong(x: ScalarQuantity[D]): Long = quantity.magnitude.toLong

    override def toFloat(x: ScalarQuantity[D]): Float = quantity.magnitude.toFloat

    override def toDouble(x: ScalarQuantity[D]): Double = quantity.magnitude

    override def compare(x: ScalarQuantity[D], y: ScalarQuantity[D]): Int = x.compare(y)

  }

}