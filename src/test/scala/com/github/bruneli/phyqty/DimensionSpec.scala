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

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author bruneli
  */
class DimensionSpec extends FlatSpec with Matchers {

  import Exponent._
  import Dimension._

  "dimensionless quantities" should "be at the origin of the vector space" in {

    dimensionVector[DimensionLess] shouldBe Vector(0, 0, 0, 0, 0, 0, 0)
    dimensionVector[PlaneAngle] shouldBe Vector(0, 0, 0, 0, 0, 0, 0)
    dimensionVector[SolidAngle] shouldBe Vector(0, 0, 0, 0, 0, 0, 0)

  }

  "base quantities" should "be the unit vectors of the physical dimension space" in {

    dimensionVector[Length] shouldBe Vector(1, 0, 0, 0, 0, 0, 0)
    dimensionVector[Mass] shouldBe Vector(0, 1, 0, 0, 0, 0, 0)
    dimensionVector[Time] shouldBe Vector(0, 0, 1, 0, 0, 0, 0)
    dimensionVector[Temperature] shouldBe Vector(0, 0, 0, 1, 0, 0, 0)
    dimensionVector[AmountOfSubstance] shouldBe Vector(0, 0, 0, 0, 1, 0, 0)
    dimensionVector[ElectricCurrent] shouldBe Vector(0, 0, 0, 0, 0, 1, 0)
    dimensionVector[LuminousIntensity] shouldBe Vector(0, 0, 0, 0, 0, 0, 1)

  }

  "quantities derived from a single base unit" should "be on the same direction than that unit" in {

    dimensionVector[Area] shouldBe Vector(2, 0, 0, 0, 0, 0, 0)
    dimensionVector[Volume] shouldBe Vector(3, 0, 0, 0, 0, 0, 0)
    dimensionVector[Frequency] shouldBe Vector(0, 0, -1, 0, 0, 0, 0)
    dimensionVector[AngularSpeed] shouldBe Vector(0, 0, -1, 0, 0, 0, 0)
    dimensionVector[AngularAcceleration] shouldBe Vector(0, 0, -2, 0, 0, 0, 0)

  }

  "quantities derived from two base units" should "be in a plane of the vector space" in {

    dimensionVector[Speed] shouldBe Vector(1, 0, -1, 0, 0, 0, 0)
    dimensionVector[Acceleration] shouldBe Vector(1, 0, -2, 0, 0, 0, 0)

  }

  "quantities derived from three base units" should "be points in the vector space" in {

    dimensionVector[Force] shouldBe Vector(1, 1, -2, 0, 0, 0, 0)
    dimensionVector[Pressure] shouldBe Vector(-1, 1, -2, 0, 0, 0, 0)
    dimensionVector[Energy] shouldBe Vector(2, 1, -2, 0, 0, 0, 0)
    dimensionVector[Power] shouldBe Vector(2, 1, -3, 0, 0, 0, 0)

  }

}
