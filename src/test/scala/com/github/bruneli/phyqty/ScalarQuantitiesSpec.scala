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
class ScalarQuantitiesSpec extends FlatSpec with Matchers {

  import PhyUnit._
  import ScalarQuantity._

  "take" should "return another list of scalar quantities" in {

    val firstTwoScalarQuantities = ScalarQuantities(3(m), 2(cm), 7(km)).take(2)

    firstTwoScalarQuantities shouldBe a[ScalarQuantities[_]]
    firstTwoScalarQuantities.length shouldBe 2
    firstTwoScalarQuantities.head shouldBe 3(m)
    firstTwoScalarQuantities.last shouldBe 2(cm)

  }

  "map" should "transform scalar quantities into others" in {

    val quantities = ScalarQuantities(3(m), 2(cm), 7(km)).map(x => (x + 1(m)) * 2)

    quantities shouldBe a[ScalarQuantities[_]]
    quantities.length shouldBe 3
    quantities.head shouldBe 8(m)
    quantities(1) shouldBe 204(cm)
    quantities.last shouldBe 14002(m)

  }

  "flatMap" should "produce new scalar quantities" in {

    val quantities = ScalarQuantities(3(m), 2(cm)).flatMap(x => ScalarQuantities(x, x * 2))

    quantities shouldBe a[ScalarQuantities[_]]
    quantities.length shouldBe 4
    quantities(0) shouldBe 3(m)
    quantities(1) shouldBe 6(m)
    quantities(2) shouldBe 2(cm)
    quantities(3) shouldBe 4(cm)

  }

  ":+" should "append a new scalar quantity" in {

    val quantities = ScalarQuantities(3(m), 2(cm)) :+ 6(mm)

    quantities shouldBe a[ScalarQuantities[_]]
    quantities.length shouldBe 3
    quantities(0) shouldBe 3(m)
    quantities(1) shouldBe 2(cm)
    quantities(2) shouldBe 6(mm)

  }

  "+:" should "prepend a new scalar quantity" in {

    val quantities = 6(mm) +: ScalarQuantities(3(m), 2(cm))

    quantities shouldBe a[ScalarQuantities[_]]
    quantities.length shouldBe 3
    quantities(0) shouldBe 6(mm)
    quantities(1) shouldBe 3(m)
    quantities(2) shouldBe 2(cm)

  }

  "+" should "perform an element wise addition of two list of scalar quantities" in {

    val q1 = ScalarQuantities(2(m), 7(m))
    val q2 = ScalarQuantities(15(cm), 1(m))
    val sum = q1 + q2

    sum shouldBe a[ScalarQuantities[_]]
    sum.length shouldBe 2
    sum(0) shouldBe 215(cm)
    sum(1) shouldBe 8(m)

  }

  it should "add a constant to a list of scalar quantities" in {

    val sum = ScalarQuantities(2(m), 7(m)) + 3(cm)

    sum shouldBe a[ScalarQuantities[_]]
    sum.length shouldBe 2
    sum(0) shouldBe 2.03(m)
    sum(1) shouldBe 703(cm)

  }

  "-" should "perform an element wise subtraction of two list of scalar quantities" in {

    val q1 = ScalarQuantities(2(m), 7(m))
    val q2 = ScalarQuantities(15(cm), 1(m))
    val diff = q1 - q2

    diff shouldBe a[ScalarQuantities[_]]
    diff.length shouldBe 2
    diff(0) shouldBe 185(cm)
    diff(1) shouldBe 6(m)

  }

  it should "subtract a constant to a list of scalar quantities" in {

    val sum = ScalarQuantities(2(m), 7(m)) - 3(cm)

    sum shouldBe a[ScalarQuantities[_]]
    sum.length shouldBe 2
    sum(0) shouldBe 197(cm)
    sum(1) shouldBe 697(cm)

  }
  
  "-ScalarQuantities(2(m), 3(cm))" should "be identical to ScalarQuantities(-2(m), -3(cm))" in {
    
    val negate = -ScalarQuantities(2(m), 3(cm))

    negate shouldBe a[ScalarQuantities[_]]
    negate.length shouldBe 2
    negate(0) shouldBe -2(m)
    negate(1) shouldBe -3(cm)
    
  }

  "*" should "perform the element-wise product of two lists of scalar quantities" in {
    
    val length = ScalarQuantities(2(m), 5(dm))
    val force = ScalarQuantities(5(N), 8(N))
    val work = force * length

    work shouldBe a[ScalarQuantities[_]]
    work.length shouldBe 2
    work(0) shouldBe 10(J)
    work(1) shouldBe 4(J)

  }

  it should "multiply every value of a list of scalar quantities by a constant" in {

    val work = ScalarQuantities(5(N), 8(N)) * 2(m)

    work shouldBe a[ScalarQuantities[_]]
    work.length shouldBe 2
    work(0) shouldBe 10(J)
    work(1) shouldBe 16(J)

  }

  it should "multiply every value of a list of scalar quantities by a scalar" in {

    val work = ScalarQuantities(5(J), 8(J)) * 2

    work shouldBe a[ScalarQuantities[_]]
    work.length shouldBe 2
    work(0) shouldBe 10(J)
    work(1) shouldBe 16(J)

  }

  "/" should "perform the element-wise division of two lists of scalar quantities" in {

    val length = ScalarQuantities(20(m), 16(dm))
    val duration = ScalarQuantities(5(s), 8(s))
    val speed = length / duration

    speed shouldBe a[ScalarQuantities[_]]
    speed.length shouldBe 2
    speed(0) shouldBe 4(m/s)
    speed(1) shouldBe 0.2(m/s)

  }

  it should "divide every value of a list of scalar quantities by a constant" in {

    val speed = ScalarQuantities(20(m), 16(m)) / 2(s)

    speed shouldBe a[ScalarQuantities[_]]
    speed.length shouldBe 2
    speed(0) shouldBe 10(m/s)
    speed(1) shouldBe 8(m/s)

  }

  it should "divide every value of a list of scalar quantities by a scalar" in {

    val speed = ScalarQuantities(20(m/s), 16(m/s)) / 2

    speed shouldBe a[ScalarQuantities[_]]
    speed.length shouldBe 2
    speed(0) shouldBe 10(m/s)
    speed(1) shouldBe 8(m/s)

  }

  "ScalarQuantities(3(m), 6(m)) in cm" should "be ScalarQuantities(300(cm), 600(cm))" in {

    val length = ScalarQuantities(3(m), 6(m)) in cm

    length shouldBe a[ScalarQuantities[_]]
    length.length shouldBe 2
    length.magnitudes(0) shouldBe 300.0 +- 1.0e-6
    length.magnitudes(1) shouldBe 600.0 +- 1.0e-6

  }

  "diff(2)" should "evaluate the difference between scalar quantities separated by 2 indexes" in {

    val length = ScalarQuantities(3(m), 6(m), 9(m), 6(m), 3(m))
    val step = length.diff(2)

    step shouldBe a[ScalarQuantities[_]]
    step.length shouldBe 5
    step.magnitude(0).isNaN shouldBe true
    step.magnitude(1).isNaN shouldBe true
    step.magnitude(2) shouldBe 6.0 +- 1.0e-6
    step.magnitude(3) shouldBe 0.0 +- 1.0e-6
    step.magnitude(4) shouldBe -6.0 +- 1.0e-6

  }

  "slice" should "build a subset of scalar quantities" in {

    val subset = ScalarQuantities(3(m), 9(m), 6(m), 12(m)).slice(1, 3)

    subset shouldBe a[ScalarQuantities[_]]
    subset.length shouldBe 2
    subset(0) shouldBe 9(m)
    subset(1) shouldBe 6(m)

  }

}
