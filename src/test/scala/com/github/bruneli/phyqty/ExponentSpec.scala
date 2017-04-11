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
class ExponentSpec extends FlatSpec with Matchers {

  import Exponent._

  "exponent" should "return the exponent as an integer" in {

    exponent[MinusSeven] shouldBe -7
    exponent[MinusSix] shouldBe -6
    exponent[MinusFive] shouldBe -5
    exponent[MinusFour] shouldBe -4
    exponent[MinusThree] shouldBe -3
    exponent[MinusTwo] shouldBe -2
    exponent[MinusOne] shouldBe -1
    exponent[Zero] shouldBe 0
    exponent[One] shouldBe 1
    exponent[Two] shouldBe 2
    exponent[Three] shouldBe 3
    exponent[Four] shouldBe 4
    exponent[Five] shouldBe 5
    exponent[Six] shouldBe 6
    exponent[Seven] shouldBe 7

  }

  "zero + zero" should "be zero" in {

    exponent[Zero + Zero] shouldBe 0

  }

  "zero - zero" should "be zero" in {

    exponent[Zero - Zero] shouldBe 0

  }

  "zero + N" should "be N" in {

    exponent[Zero + One] shouldBe 1
    exponent[Zero + Three] shouldBe 3
    exponent[Zero + MinusSix] shouldBe -6

  }

  "N + 0" should "be N" in {

    exponent[One + Zero] shouldBe 1
    exponent[Three + Zero] shouldBe 3
    exponent[MinusSix + Zero] shouldBe -6

  }

  "zero - N" should "be the opposite of N" in {

    exponent[Zero - One] shouldBe -1
    exponent[Zero - Three] shouldBe -3
    exponent[Zero - MinusFive] shouldBe 5

  }

  "N - zero" should "be N" in {

    exponent[One - Zero] shouldBe 1
    exponent[Three - Zero] shouldBe 3
    exponent[MinusFive - Zero] shouldBe -5

  }

  "6 - 3" should "be 3" in {

    exponent[Six - Three] shouldBe 3

  }

  "6 + (-2)" should "be 4" in {

    exponent[Six + MinusTwo] shouldBe 4

  }

  "-6 + 3" should "be -3" in {

    exponent[MinusSix + Three] shouldBe -3

  }

  "-5 + (-2)" should "be -7" in {

    exponent[MinusFive + MinusTwo] shouldBe -7

  }

}
