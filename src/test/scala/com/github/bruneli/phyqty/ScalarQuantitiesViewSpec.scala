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
import PhyUnit._
import ScalarQuantity._

import scala.collection.SeqView

/**
  * @author bruneli
  */
class ScalarQuantitiesViewSpec extends FlatSpec with Matchers {

  "+" should "perform an element wise addition of two list of scalar quantities" in {

    val q1 = ScalarQuantities(2(m), 7(m))
    val q2 = ScalarQuantities(15(cm), 1(m))
    val sum = q1.view + q2

    sum shouldBe a[ScalarQuantitiesView[_, _]]
    sum.length shouldBe 2
    sum(0) shouldBe 215(cm)
    sum(1) shouldBe 8(m)

  }

  it should "add a constant to a list of scalar quantities" in {

    val sum = ScalarQuantities(2(m), 7(m)).view + 3(cm)

    sum shouldBe a[ScalarQuantitiesView[_, _]]
    sum.length shouldBe 2
    sum(0) shouldBe 2.03(m)
    sum(1) shouldBe 703(cm)

  }

  "-" should "perform an element wise subtraction of two list of scalar quantities" in {

    val q1 = ScalarQuantities(2(m), 7(m))
    val q2 = ScalarQuantities(15(cm), 1(m))
    val diff = q1.view - q2

    diff shouldBe a[ScalarQuantitiesView[_, _]]
    diff.length shouldBe 2
    diff(0) shouldBe 185(cm)
    diff(1) shouldBe 6(m)

  }

  it should "subtract a constant to a list of scalar quantities" in {

    val diff = ScalarQuantities(2(m), 7(m)).view - 3(cm)

    diff shouldBe a[ScalarQuantitiesView[_, _]]
    diff.length shouldBe 2
    diff(0) shouldBe 197(cm)
    diff(1) shouldBe 697(cm)

  }

  "-ScalarQuantities(2(m), 3(cm))" should "be identical to scalar quantities(-2(m), -3(cm))" in {

    val length = ScalarQuantities(2(m), 3(cm)).view
    val negate = -length

    negate shouldBe a[ScalarQuantitiesView[_, _]]
    negate.length shouldBe 2
    negate(0) shouldBe -2(m)
    negate(1) shouldBe -3(cm)

  }

  "*" should "perform the element-wise product of two list of scalar quantities" in {

    val length = ScalarQuantities(2(m), 5(dm))
    val force = ScalarQuantities(5(N), 8(N))
    val work = force.view * length

    work shouldBe a[ScalarQuantitiesView[_, _]]
    work.length shouldBe 2
    work(0) shouldBe 10(J)
    work(1) shouldBe 4(J)

  }

  it should "multiply every value of a list of scalar quantities by a constant" in {

    val work = ScalarQuantities(5(N), 8(N)).view * 2(m)

    work shouldBe a[ScalarQuantitiesView[_, _]]
    work.length shouldBe 2
    work(0) shouldBe 10(J)
    work(1) shouldBe 16(J)

  }

  it should "multiply every value of a list of scalar quantities by a scalar" in {

    val work = ScalarQuantities(5(J), 8(J)).view * 2

    work shouldBe a[ScalarQuantitiesView[_, _]]
    work.length shouldBe 2
    work(0) shouldBe 10(J)
    work(1) shouldBe 16(J)

  }

  "/" should "perform the element-wise division of two list of scalar quantities" in {

    val length = ScalarQuantities(20(m), 16(dm))
    val duration = ScalarQuantities(5(s), 8(s))
    val speed = length.view / duration

    speed shouldBe a[ScalarQuantitiesView[_, _]]
    speed.length shouldBe 2
    speed(0) shouldBe 4(m/s)
    speed(1) shouldBe 0.2(m/s)

  }

  it should "divide every value of a list of scalar quantities by a constant" in {

    val speed = ScalarQuantities(20(m), 16(m)).view / 2(s)

    speed shouldBe a[ScalarQuantitiesView[_, _]]
    speed.length shouldBe 2
    speed(0) shouldBe 10(m/s)
    speed(1) shouldBe 8(m/s)

  }

  it should "divide every value of a list of scalar quantities by a scalar" in {

    val speed = ScalarQuantities(20(m/s), 16(m/s)).view / 2

    speed shouldBe a[ScalarQuantitiesView[_, _]]
    speed.length shouldBe 2
    speed(0) shouldBe 10(m/s)
    speed(1) shouldBe 8(m/s)

  }

  "ScalarQuantitiesView(3(m), 6(m)) in cm" should "be ScalarQuantitiesView(300(cm), 600(cm))" in {

    val length = ScalarQuantities(3(m), 6(m)).view in cm

    length shouldBe a[ScalarQuantitiesView[_, _]]
    length.length shouldBe 2
    length.magnitude(0) shouldBe 300.0 +- 1.0e-6
    length.magnitude(1) shouldBe 600.0 +- 1.0e-6

  }

  "slice" should "build a subset of scalar quantities" in {

    val subset = ScalarQuantities(3(m), 9(m), 6(m), 12(m)).view.slice(1, 3)

    subset shouldBe a[ScalarQuantitiesView[_, _]]
    subset.length shouldBe 2
    subset(0) shouldBe 9(m)
    subset(1) shouldBe 6(m)

  }

  "map" should "transform ScalarQuantities into a view sequence" in {

    val length = ScalarQuantities(4(m), 2(m), 5(m)).view
    val deltaT = 2(s)
    val speed = length.map(_ / deltaT)

    speed shouldBe a[SeqView[_, _]]
    speed.length shouldBe 3
    speed(0).magnitude shouldBe 2.0 +- 1.0e-6
    speed(1).magnitude shouldBe 1.0 +- 1.0e-6
    speed(2).magnitude shouldBe 2.5 +- 1.0e-6

  }

}
