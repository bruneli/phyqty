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

/**
  * @author bruneli
  */
trait PhyUnitConverter {

  def value(magnitude: Double): Double

  def inverse(magnitude: Double, that: PhyUnitConverter): Double

  def symbol: String

  def in(that: PhyUnitConverter): Double

  def * (that: PhyUnitConverter): PhyUnitConverter

  def / (that: PhyUnitConverter): PhyUnitConverter

}

trait LinearTransform extends PhyUnitConverter {

  def in(that: PhyUnitConverter): Double = that match {
    case linear: LinearTransform => (this / linear).value(1.0)
    case _ => ???
  }

  def apply[D <: Dimension[_, _, _, _, _, _, _]](unit: PhyUnit[D]): PhyUnit[D] = {
    val symbol = s"${this.symbol}${unit.symbol}"
    unit.copy(symbol = symbol, converter = unit.converter * this)
  }

  override def inverse(magnitude: Double, that: PhyUnitConverter): Double = that match {
    case linear: LinearTransform => (that / this).value(magnitude)
    case _ => magnitude / value(1.0)
  }

  override def *(that: PhyUnitConverter): PhyUnitConverter = that match {
    case IdentityConverter =>
      this
    case linear: LinearTransform =>
      LinearMultiplier(s"$symbol${that.symbol}", this.value(1.0) * that.value(1.0))
    case _ => ???
  }

  override def /(that: PhyUnitConverter): PhyUnitConverter = that match {
    case IdentityConverter =>
      this
    case linear: LinearTransform =>
      LinearMultiplier(s"$symbol/${that.symbol}", this.value(1.0) / that.value(1.0))
    case _ => ???
  }

  def inverseTransform: LinearTransform

}











case class AffineTransform(symbol: String, slope: Double, offset: Double) extends PhyUnitConverter {
  require(slope != 0.0, "slope should be different from zero")

  override def value(magnitude: Double): Double = slope * magnitude + offset

  override def inverse(magnitude: Double, that: PhyUnitConverter): Double = {
    (that.value(magnitude) - offset) / slope
  }

  override def in(that: PhyUnitConverter): Double = ???

  override def *(that: PhyUnitConverter): PhyUnitConverter = ???

  override def /(that: PhyUnitConverter): PhyUnitConverter = ???

  override def equals(obj: Any): Boolean = obj match {
    case that: AffineTransform => that.slope == this.slope && that.offset == this.offset
    case _ => false
  }

}