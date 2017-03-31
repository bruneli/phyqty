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
class MathSpec extends FlatSpec with Matchers {

  import PhyUnit._
  import Quantity._
  import Math._

  "abs" should "return the absolute value of a quantity" in {

    abs(-4(m)) shouldBe 4(m)
    abs(Quantities(-4(m), 2(cm), -8(mm))) shouldBe Quantities(4(m), 2(cm), 8(mm))

  }

  "exp" should "perform the exponential of a dimensionless quantity" in {

    exp(4(m) / 2(m)).magnitude shouldBe math.exp(2.0) +- 1.0e-6
    val expQ = exp(Quantities(2(rad), 4(rad)))
    expQ.length shouldBe 2
    expQ.magnitude(0) shouldBe math.exp(2.0) +- 1.0e-6
    expQ.magnitude(1) shouldBe math.exp(4.0) +- 1.0e-6

  }

  "log" should "perform the logarithm of a dimensionless quantity" in {

    log(4(m) / 2(m)).magnitude shouldBe math.log(2.0) +- 1.0e-6
    val logQ = log(Quantities(2(rad), 4(rad)).view)
    logQ.length shouldBe 2
    logQ.magnitude(0) shouldBe math.log(2.0) +- 1.0e-6
    logQ.magnitude(1) shouldBe math.log(4.0) +- 1.0e-6

  }

}
