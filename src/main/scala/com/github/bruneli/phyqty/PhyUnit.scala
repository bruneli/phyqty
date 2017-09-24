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

import Dimension._
import Exponent._

/**
  * @author bruneli
  */
case class PhyUnit[D <: Dimension[_, _, _, _, _, _, _]](symbol: String, converter: PhyUnitConverter = IdentityConverter) {

  def * [DD <: Dimension[_, _, _, _, _, _, _]](that: PhyUnit[DD]): PhyUnit[D x DD] = {
    PhyUnit[D x DD](s"${this.symbol}*${that.symbol}", this.converter * that.converter)
  }

  def / [DD <: Dimension[_, _, _, _, _, _, _]](that: PhyUnit[DD]): PhyUnit[D / DD] = {
    PhyUnit[D / DD](s"${this.symbol}/${that.symbol}", this.converter / that.converter)
  }

  def *: (magnitude: Double): ScalarQuantity[D] = {
    ScalarQuantity(magnitude, this)
  }

  def *: (magnitude: Int): ScalarQuantity[D] = {
    ScalarQuantity(magnitude.toDouble, this)
  }

  def named(symbol: String): PhyUnit[D] = {
    copy(symbol = symbol)
  }

  override def equals(obj: Any): Boolean = obj match {
    case unit: PhyUnit[D] => converter == unit.converter
    case _ => false
  }

  override def toString: String = s"$symbol"

}

object PhyUnit {

  import Dimension._
  import DecimalMultiplier._
  import LinearMultiplier._

  val radian, rad = PhyUnit[PlaneAngle]("rad")
  val steradian, sr = PhyUnit[SolidAngle]("sr")

  val metre, meter, m = PhyUnit[Length]("m")
  val kilogram, kg = PhyUnit[Mass]("kg")
  val second, s = PhyUnit[Time]("s")
  val kelvin, K = PhyUnit[Temperature]("K")
  val mole, mol = PhyUnit[AmountOfSubstance]("mol")
  val ampere, A = PhyUnit[ElectricCurrent]("A")
  val candela, cd = PhyUnit[LuminousIntensity]("cd")

  val hertz, Hz = PhyUnit[Frequency]("Hz")
  val becquerel, Bq = PhyUnit[RadioactiveActivity]("Bq")
  val gray, Gy = PhyUnit[RadioactiveDose]("Gy")
  val lumen, lm = PhyUnit[LuminousFlux]("lm")
  val lux, lx = PhyUnit[Illuminance]("lx")
  val katal = PhyUnit[CatalyticActivity]("katal")
  val newton, N = PhyUnit[Force]("N")
  val pascal, Pa = PhyUnit[Pressure]("Pa")
  val joule, J = PhyUnit[Energy]("J")
  val watt, W = PhyUnit[Power]("W")
  val coulomb, C = PhyUnit[ElectricCharge]("C")
  val volt, V = PhyUnit[ElectricPotential]("V")
  val farad, F = PhyUnit[Capacitance]("F")
  val ohm = PhyUnit[ElectricalResistance]("ohm")
  val siemens, S = PhyUnit[ElectricalConductance]("S")
  val tesla, T = PhyUnit[MagneticField]("T")
  val weber, Wb = PhyUnit[MagneticFlux]("Wb")
  val henry, H = PhyUnit[Inductance]("H")

  // Units derived from meter
  val nanometre, nm = nano(metre)
  val micrometre, micron = micro(metre)
  val millimetre, mm = milli(metre)
  val centimetre, cm = centi(metre)
  val decimetre, dm = deci(metre)
  val kilometre, km = kilo(metre)

  // Units derived from kilogram
  val gram, g = milli(kilogram).named("g")
  val milligram, mg = milli(gram)

  // Units derived from second
  val nanosecond, ns = nano(second)
  val microsecond = micro(second)
  val millisecond, ms = milli(second)
  val minute, min = sixtyTimes(second).named("min")
  val hour, h = sixtyTimes(minute).named("h")
  val day = twentyFourTimes(hour).named("day")

  // Temperature units
  val degC, degreeCelsius = PhyUnit[Temperature]("degC", AffineTransform("", 1.0, 273.15))
  val degF, degreeFahrenheit = PhyUnit[Temperature]("degF", AffineTransform("", 5.0 / 9.0, 459.67 * 5.0 / 9.0))

  def unitDimension[L <: Exponent, M <: Exponent, T <: Exponent, K <: Exponent, N <: Exponent, I <: Exponent, J <: Exponent](unit: PhyUnit[Dimension[L, M, T, K, N, I, J]])(
    implicit lengthToInt: ExponentToInt[L], massToInt: ExponentToInt[M], timeToInt: ExponentToInt[T], temperatureToInt: ExponentToInt[K], amountOfSubstanceToInt: ExponentToInt[N], electricCurrentToInt: ExponentToInt[I], luminousIntensityToInt: ExponentToInt[J]): Vector[Int] = {
    dimensionVector[Dimension[L, M, T, K, N, I, J]]
  }

}
