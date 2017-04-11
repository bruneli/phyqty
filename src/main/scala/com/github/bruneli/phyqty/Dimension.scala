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

import Exponent._

/**
  * Dimension exponents used to build new physical dimension from existing one
  *
  * @author bruneli
  */
sealed trait DimensionExponents {

  type LENGTH <: Exponent

  type MASS <: Exponent

  type TIME <: Exponent

  type TEMPERATURE <: Exponent

  type AMOUNT_OF_SUBSTANCE <: Exponent

  type CURRENT <: Exponent

  type LUMINOSITY <: Exponent

  type TIMES[D <: DimensionExponents] <: Dimension[_, _, _, _, _, _, _]

  type OVER[D <: DimensionExponents] <: Dimension[_, _, _, _, _, _, _]

}

/**
  * Dimension of a physical quantity
  *
  * @tparam L Length dimension exponent
  * @tparam M Mass dimension exponent
  * @tparam T Time dimension exponent
  * @tparam K Temperature dimension exponent (usually represented as Theta)
  * @tparam N Amount of substance dimension exponent
  * @tparam I Electric current dimension exponent
  * @tparam J Luminous intensity dimension exponent
  */
class Dimension[L <: Exponent, M <: Exponent, T <: Exponent, K <: Exponent, N <: Exponent, I <: Exponent, J <: Exponent]
  extends DimensionExponents {

  type LENGTH = L

  type MASS = M

  type TIME = T

  type TEMPERATURE = K

  type AMOUNT_OF_SUBSTANCE = N

  type CURRENT = I

  type LUMINOSITY = J

  type TIMES[D <: DimensionExponents] = Dimension[
    LENGTH + D#LENGTH,
    MASS + D#MASS,
    TIME + D#TIME,
    TEMPERATURE + D#TEMPERATURE,
    AMOUNT_OF_SUBSTANCE + D#AMOUNT_OF_SUBSTANCE,
    CURRENT + D#CURRENT,
    LUMINOSITY + D#LUMINOSITY]

  type OVER[D <: DimensionExponents] = Dimension[
    LENGTH - D#LENGTH,
    MASS - D#MASS,
    TIME - D#TIME,
    TEMPERATURE - D#TEMPERATURE,
    AMOUNT_OF_SUBSTANCE - D#AMOUNT_OF_SUBSTANCE,
    CURRENT - D#CURRENT,
    LUMINOSITY - D#LUMINOSITY]

}

object Dimension {

  type x[L <: DimensionExponents, R <: DimensionExponents] = L#TIMES[R]
  type /[L <: DimensionExponents, R <: DimensionExponents] = L#OVER[R]

  type DimensionLess = Dimension[Zero, Zero, Zero, Zero, Zero, Zero, Zero]
  type PlaneAngle = DimensionLess
  type SolidAngle = DimensionLess

  // SI base quantities
  type Length = Dimension[One, Zero, Zero, Zero, Zero, Zero, Zero]
  type Mass = Dimension[Zero, One, Zero, Zero, Zero, Zero, Zero]
  type Time = Dimension[Zero, Zero, One, Zero, Zero, Zero, Zero]
  type Temperature = Dimension[Zero, Zero, Zero, One, Zero, Zero, Zero]
  type AmountOfSubstance = Dimension[Zero, Zero, Zero, Zero, One, Zero, Zero]
  type ElectricCurrent = Dimension[Zero, Zero, Zero, Zero, Zero, One, Zero]
  type LuminousIntensity = Dimension[Zero, Zero, Zero, Zero, Zero, Zero, One]

  // Derived quantity built from one base quantity
  type Area = Length x Length
  type Volume = Area x Length
  type Frequency = DimensionLess / Time
  type AngularSpeed = PlaneAngle / Time
  type AngularAcceleration = AngularSpeed / Time
  type RadioactiveActivity = DimensionLess / Time
  type LuminousFlux = LuminousIntensity x SolidAngle

  // Derived quantity built from two base quantities
  type Speed = Length / Time
  type Acceleration = Speed / Time
  type ElectricCharge = ElectricCurrent x Time
  type CatalyticActivity = AmountOfSubstance / Time
  type Illuminance = LuminousFlux / Area

  // Derived quantity built from three base quantities
  type Momentum = Mass x Speed
  type Force = Mass x Acceleration
  type Pressure = Force / Area
  type Energy = Force x Length
  type Power = Energy / Time
  type RadioactiveDose = Energy / Mass

  // Derived quantity built from four or more base quantities
  type Entropy = Energy / Temperature
  type ElectricField = Force / ElectricCharge
  type ElectricPotential = ElectricField x Length
  type Capacitance = ElectricCharge / ElectricPotential
  type ElectricalResistance = ElectricPotential / ElectricCurrent
  type ElectricalConductance = DimensionLess / ElectricalResistance
  type MagneticField = Force / ElectricCharge / Speed
  type MagneticFlux = MagneticField x Area
  type Inductance = MagneticFlux / ElectricCurrent

  def dimensionVector[D <: DimensionExponents](implicit lengthToInt: ExponentToInt[D#LENGTH], massToInt: ExponentToInt[D#MASS], timeToInt: ExponentToInt[D#TIME], temperatureToInt: ExponentToInt[D#TEMPERATURE], amountOfSubstanceToInt: ExponentToInt[D#AMOUNT_OF_SUBSTANCE], electricCurrentToInt: ExponentToInt[D#CURRENT], luminousIntensityToInt: ExponentToInt[D#LUMINOSITY]): Vector[Int] = {
    Vector(exponent[D#LENGTH], exponent[D#MASS], exponent[D#TIME], exponent[D#TEMPERATURE], exponent[D#AMOUNT_OF_SUBSTANCE], exponent[D#CURRENT], exponent[D#LUMINOSITY])
  }

}