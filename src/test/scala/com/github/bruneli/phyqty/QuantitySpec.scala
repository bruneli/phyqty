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
class QuantitySpec extends FlatSpec with Matchers {

  import PhyUnit._
  import Quantity._

  "4 m/s + 3 m/s" should "return 7 m/s" in {

    4 *: (m/s) + 3 *: (m/s) shouldBe (7 *: (m/s))

  }

  "4 kg/s*m/s + 3 N" should "be 7 N" in {

    (4 *: N) + (3 *: (kg/s*m/s)) shouldBe (7 *: N)

    (5 *: J) + (5 *: (m*N)) shouldBe (10 *: J)

  }

  "3 W - 2 J/s" should "be 1 W" in {

    3(W) - 2(J/s) shouldBe 1(W)

  }

  "3 m * 2 m * 4 m" should "be 24 m3" in {

    3(m) * 2(m) * 4(m) shouldBe 24(m*m*m)

  }

  "-2 m/s" should "have magnitude -2" in {

    val speed = 2(m/s)
    -speed.magnitude shouldBe -2.0 +- 1.0e-6

  }

  "3 m/s * 2" should "be 6 m/s" in {

    3(m/s) * 2 shouldBe 6(m/s)

  }

  "6 m/s / 2" should "be 3 m/s" in {

    6(m/s) / 2 shouldBe 3(m/s)

  }

  "10 m / 5 s" should "be 2 m/s" in {

    10(m) / 5(s) shouldBe 2(m/s)

  }

  "2 m in cm" should "be equal to 200 cm" in {

    2(metre) in centimetre shouldBe 200(cm)

  }

  "2 cm in km" should "be equal to 2 x 10-5" in {

    2(cm).in(km).magnitude shouldBe 2.0e-5 +- 1.0e-8

  }

  "11 cm in cm" should "be equal to 11" in {

    11(cm).in(cm).magnitude shouldBe 11.0 +- 1.0e-8

  }

}
