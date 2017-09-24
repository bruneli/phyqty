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
class PhyUnitSpec extends FlatSpec with Matchers {

  import PhyUnit._
  import ScalarQuantity._
  import DecimalMultiplier._

  "m" should "be equal to metre but different from centimetre" in {

    m shouldEqual metre
    m should not equal centimetre

  }

  "milli(millimetre)" should "be equal to a micrometre" in {

    milli(millimetre) shouldEqual micrometre

  }

  "unitDimension" should "be (1,1,-2) for a Newton" in {

    unitDimension(newton) shouldBe Vector(1, 1, -2, 0, 0, 0, 0)

  }

  "20 degC" should "be equal to 20 + 273.15 K" in {

    20 (degC).in(K) shouldBe (273.15 + 20) (K)

  }

  "1 min" should "be equal to 60 second" in {

    1 (min).in(s) shouldBe 60 (s)

  }

  "2 hour" should "be equal to 7200 second" in {

    2 (hour).in(s) shouldBe 7200 (s)

  }

}
