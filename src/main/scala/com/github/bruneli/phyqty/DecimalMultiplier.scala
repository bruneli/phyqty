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
case class DecimalMultiplier(symbol: String, exponent: Int) extends LinearTransform {

  def value(magnitude: Double): Double = magnitude * math.pow(10.0, exponent)

  override def inverseTransform: DecimalMultiplier = DecimalMultiplier(s"1/$symbol", -exponent)

  override def *(that: PhyUnitConverter): PhyUnitConverter = that match {
    case IdentityConverter =>
      this
    case decimal: DecimalMultiplier =>
      if (this.exponent + decimal.exponent == 0) {
        IdentityConverter
      } else {
        DecimalMultiplier(s"$symbol${that.symbol}", this.exponent + decimal.exponent)
      }
    case linear: LinearMultiplier =>
      LinearMultiplier(s"$symbol${that.symbol}", this.value(1.0) * linear.multiplier)
    case _ => ???
  }

  override def /(that: PhyUnitConverter): PhyUnitConverter = that match {
    case IdentityConverter =>
      this
    case decimal: DecimalMultiplier =>
      if (this.exponent - decimal.exponent == 0) {
        IdentityConverter
      } else {
        DecimalMultiplier(s"$symbol${that.symbol}", this.exponent - decimal.exponent)
      }
    case linear: LinearMultiplier =>
      LinearMultiplier(s"$symbol/${that.symbol}", this.value(1.0) / linear.multiplier)
    case _ => ???
  }

  override def equals(obj: Any): Boolean = obj match {
    case DecimalMultiplier(thatSymbol, thatExponent) => this.exponent == thatExponent
    case _ => false
  }

}

object DecimalMultiplier {

  val (eta, femto) = coupledDecimalMultipliers("E", "f", 15)
  val (tera, pico) = coupledDecimalMultipliers("G", "p", 12)
  val (giga, nano) = coupledDecimalMultipliers("T", "n", 9)
  val (mega, micro) = coupledDecimalMultipliers("M", "micro", 6)
  val (kilo, milli) = coupledDecimalMultipliers("k", "m", 3)
  val (hecto, centi) = coupledDecimalMultipliers("hecto", "c", 2)
  val (deca, deci) = coupledDecimalMultipliers("deca", "d", 1)

  def coupledDecimalMultipliers(symbol: String, inverseSymbol: String, exponent: Int) = {
    val multiplier = DecimalMultiplier(symbol, exponent)
    val inverse = DecimalMultiplier(inverseSymbol, -exponent)
    multiplier -> inverse
  }

}