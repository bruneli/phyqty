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
class QuantitiesSpec extends FlatSpec with Matchers {

  import PhyUnit._
  import Quantity._

  "take" should "return another list of quantities" in {

    val firstTwoQuantities = Quantities(3(m), 2(cm), 7(km)).take(2)

    firstTwoQuantities shouldBe a[Quantities[_]]
    firstTwoQuantities.length shouldBe 2
    firstTwoQuantities.head shouldBe 3(m)
    firstTwoQuantities.last shouldBe 2(cm)

  }

  "map" should "transform quantities into others" in {

    val quantities = Quantities(3(m), 2(cm), 7(km)).map(x => (x + 1(m)) * 2)

    quantities shouldBe a[Quantities[_]]
    quantities.length shouldBe 3
    quantities.head shouldBe 8(m)
    quantities(1) shouldBe 204(cm)
    quantities.last shouldBe 14002(m)

  }

  "flatMap" should "produce new quantities" in {

    val quantities = Quantities(3(m), 2(cm)).flatMap(x => Quantities(x, x * 2))

    quantities shouldBe a[Quantities[_]]
    quantities.length shouldBe 4
    quantities(0) shouldBe 3(m)
    quantities(1) shouldBe 6(m)
    quantities(2) shouldBe 2(cm)
    quantities(3) shouldBe 4(cm)

  }

  ":+" should "append a new Quantity" in {

    val quantities = Quantities(3(m), 2(cm)) :+ 6(mm)

    quantities shouldBe a[Quantities[_]]
    quantities.length shouldBe 3
    quantities(0) shouldBe 3(m)
    quantities(1) shouldBe 2(cm)
    quantities(2) shouldBe 6(mm)

  }

  "+:" should "prepend a new Quantity" in {

    val quantities = 6(mm) +: Quantities(3(m), 2(cm))

    quantities shouldBe a[Quantities[_]]
    quantities.length shouldBe 3
    quantities(0) shouldBe 6(mm)
    quantities(1) shouldBe 3(m)
    quantities(2) shouldBe 2(cm)

  }

  "+" should "perform an element wise addition of two list of quantities" in {

    val q1 = Quantities(2(m), 7(m))
    val q2 = Quantities(15(cm), 1(m))
    val sum = q1 + q2

    sum shouldBe a[Quantities[_]]
    sum.length shouldBe 2
    sum(0) shouldBe 215(cm)
    sum(1) shouldBe 8(m)

  }

  it should "add a constant to a list of quantities" in {

    val sum = Quantities(2(m), 7(m)) + 3(cm)

    sum shouldBe a[Quantities[_]]
    sum.length shouldBe 2
    sum(0) shouldBe 2.03(m)
    sum(1) shouldBe 703(cm)

  }

  "-" should "perform an element wise subtraction of two list of quantities" in {

    val q1 = Quantities(2(m), 7(m))
    val q2 = Quantities(15(cm), 1(m))
    val diff = q1 - q2

    diff shouldBe a[Quantities[_]]
    diff.length shouldBe 2
    diff(0) shouldBe 185(cm)
    diff(1) shouldBe 6(m)

  }

  it should "subtract a constant to a list of quantities" in {

    val sum = Quantities(2(m), 7(m)) - 3(cm)

    sum shouldBe a[Quantities[_]]
    sum.length shouldBe 2
    sum(0) shouldBe 197(cm)
    sum(1) shouldBe 697(cm)

  }
  
  "-Quantities(2(m), 3(cm))" should "be identical to Quantities(-2(m), -3(cm))" in {
    
    val negate = -Quantities(2(m), 3(cm))

    negate shouldBe a[Quantities[_]]
    negate.length shouldBe 2
    negate(0) shouldBe -2(m)
    negate(1) shouldBe -3(cm)
    
  }

  "*" should "perform the element-wise product of two list of quantities" in {
    
    val length = Quantities(2(m), 5(dm))
    val force = Quantities(5(N), 8(N))
    val work = force * length

    work shouldBe a[Quantities[_]]
    work.length shouldBe 2
    work(0) shouldBe 10(J)
    work(1) shouldBe 4(J)

  }

  it should "multiply every value of a list of quantities by a constant" in {

    val work = Quantities(5(N), 8(N)) * 2(m)

    work shouldBe a[Quantities[_]]
    work.length shouldBe 2
    work(0) shouldBe 10(J)
    work(1) shouldBe 16(J)

  }

  it should "multiply every value of a list of quantities by a scalar" in {

    val work = Quantities(5(J), 8(J)) * 2

    work shouldBe a[Quantities[_]]
    work.length shouldBe 2
    work(0) shouldBe 10(J)
    work(1) shouldBe 16(J)

  }

  "/" should "perform the element-wise division of two list of quantities" in {

    val length = Quantities(20(m), 16(dm))
    val duration = Quantities(5(s), 8(s))
    val speed = length / duration

    speed shouldBe a[Quantities[_]]
    speed.length shouldBe 2
    speed(0) shouldBe 4(m/s)
    speed(1) shouldBe 0.2(m/s)

  }

  it should "divide every value of a list of quantities by a constant" in {

    val speed = Quantities(20(m), 16(m)) / 2(s)

    speed shouldBe a[Quantities[_]]
    speed.length shouldBe 2
    speed(0) shouldBe 10(m/s)
    speed(1) shouldBe 8(m/s)

  }

  it should "divide every value of a list of quantities by a scalar" in {

    val speed = Quantities(20(m/s), 16(m/s)) / 2

    speed shouldBe a[Quantities[_]]
    speed.length shouldBe 2
    speed(0) shouldBe 10(m/s)
    speed(1) shouldBe 8(m/s)

  }

  "Quantities(3(m), 6(m)) in cm" should "be Quantities(300(cm), 600(cm))" in {

    val length = Quantities(3(m), 6(m)) in cm

    length shouldBe a[Quantities[_]]
    length.length shouldBe 2
    length.magnitudes(0) shouldBe 300.0 +- 1.0e-6
    length.magnitudes(1) shouldBe 600.0 +- 1.0e-6

  }

  "diff(2)" should "evaluate the difference between quantities separated by 2 indexes" in {

    val length = Quantities(3(m), 6(m), 9(m), 6(m), 3(m))
    val step = length.diff(2)

    step shouldBe a[Quantities[_]]
    step.length shouldBe 5
    step.magnitude(0).isNaN shouldBe true
    step.magnitude(1).isNaN shouldBe true
    step.magnitude(2) shouldBe 6.0 +- 1.0e-6
    step.magnitude(3) shouldBe 0.0 +- 1.0e-6
    step.magnitude(4) shouldBe -6.0 +- 1.0e-6

  }

  "slice" should "build a subset of quantities" in {

    val subset = Quantities(3(m), 9(m), 6(m), 12(m)).slice(1, 3)

    subset shouldBe a[Quantities[_]]
    subset.length shouldBe 2
    subset(0) shouldBe 9(m)
    subset(1) shouldBe 6(m)

  }

}
