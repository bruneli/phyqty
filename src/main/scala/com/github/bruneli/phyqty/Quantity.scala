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
case class Quantity[D <: Dimension[_, _, _, _, _, _, _]](magnitude: Double, unit: PhyUnit[D]) extends Ordered[Quantity[D]] {

  def + (that: Quantity[D]): Quantity[D] = {
    if (that.unit.converter == this.unit.converter) {
      Quantity(this.magnitude + that.magnitude, unit)
    } else {
      Quantity(this.magnitude + that.in(unit).magnitude, unit)
    }
  }

  def - (that: Quantity[D]): Quantity[D] = {
    if (that.unit.converter == this.unit.converter) {
      Quantity(this.magnitude - that.magnitude, unit)
    } else {
      Quantity(this.magnitude - that.in(unit).magnitude, unit)
    }
  }

  def unary_-(): Quantity[D] = {
    Quantity(-magnitude, unit)
  }

  def * (scalar: Double): Quantity[D] = {
    Quantity(magnitude * scalar, unit)
  }

  def * [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD]): Quantity[D x DD] = {
    Quantity(this.magnitude * that.magnitude, this.unit * that.unit)
  }

  def / (scalar: Double): Quantity[D] = {
    Quantity(magnitude / scalar, unit)
  }

  def / [DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD]): Quantity[D / DD] = {
    Quantity(this.magnitude / that.magnitude, this.unit / that.unit)
  }

  def in(anotherUnit: PhyUnit[D]): Quantity[D] = {
    Quantity(anotherUnit.converter.inverse(magnitude, unit.converter), anotherUnit)
  }

  override def toString: String = s"$magnitude $unit"

  override def compare(that: Quantity[D]): Int = {
    this.magnitude.compare(that.in(this.unit).magnitude)
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Quantity[D] => this.compare(that) == 0
    case _ => false
  }

}

object Quantity {

  implicit class DoubleToQuantity(val magnitude: Double) extends AnyVal {

    def apply[D <: Dimension[_, _, _, _, _, _, _]](unit: PhyUnit[D]): Quantity[D] = {
      Quantity(magnitude, unit)
    }

  }

  implicit class IntToQuantity(val magnitude: Int) extends AnyVal {

    def apply[D <: Dimension[_, _, _, _, _, _, _]](unit: PhyUnit[D]): Quantity[D] = {
      Quantity(magnitude, unit)
    }

  }

  implicit class QuantityIsNumeric[D <: Dimension[_, _, _, _, _, _, _]](quantity: Quantity[D]) extends Numeric[Quantity[D]] {

    override def plus(x: Quantity[D], y: Quantity[D]): Quantity[D] = x + y

    override def minus(x: Quantity[D], y: Quantity[D]): Quantity[D] = x - y

    override def times(x: Quantity[D], y: Quantity[D]): Quantity[D] = ???

    override def negate(x: Quantity[D]): Quantity[D] = -quantity

    override def fromInt(x: Int): Quantity[D] = ???

    override def toInt(x: Quantity[D]): Int = quantity.magnitude.toInt

    override def toLong(x: Quantity[D]): Long = quantity.magnitude.toLong

    override def toFloat(x: Quantity[D]): Float = quantity.magnitude.toFloat

    override def toDouble(x: Quantity[D]): Double = quantity.magnitude

    override def compare(x: Quantity[D], y: Quantity[D]): Int = x.compare(y)

  }

}