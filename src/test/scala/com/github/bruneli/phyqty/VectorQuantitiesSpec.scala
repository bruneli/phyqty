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
class VectorQuantitiesSpec extends FlatSpec with Matchers {

  import PhyUnit._
  import ScalarQuantity._

  val v1 = VectorQuantity(2(m), 4(m))
  val v2 = VectorQuantity(7(m), 14(m))
  val v3 = VectorQuantity(15(cm), 30(cm))
  val v4 = VectorQuantity(1(m), 0.5(m))

  "take" should "return another list of vector quantities" in {

    val x = ScalarQuantities(3(m), 2(cm), 7(km))
    val y = ScalarQuantities(5(m), 2(m), 50(m))
    val firstTwoVectorQuantities = VectorQuantities(x, y).take(2)

    firstTwoVectorQuantities shouldBe a[VectorQuantities[_, _]]
    firstTwoVectorQuantities.length shouldBe 2
    firstTwoVectorQuantities.head shouldBe VectorQuantity(3(m), 5(m))
    firstTwoVectorQuantities.last shouldBe VectorQuantity(2(cm), 2(m))

  }

  "map" should "transform vector quantities into others" in {

    val x = ScalarQuantities(3(m), 2(cm), 7(km))
    val y = ScalarQuantities(5(m), 2(m), 50(m))
    val z = ScalarQuantities(10(m), 3(m), 100(m))
    val quantities = VectorQuantities(x, y, z).map(v => v * 2)

    quantities shouldBe a[VectorQuantities[_, _]]
    quantities.length shouldBe 3
    quantities.head shouldBe VectorQuantity(6(m), 10(m), 20(m))
    quantities(1) shouldBe VectorQuantity(4(cm), 4(m), 6(m))
    quantities.last shouldBe VectorQuantity(14(km), 100(m), 200(m))

  }

  "flatMap" should "produce new vector quantities" in {

    val x = ScalarQuantities(3(m), 2(cm))
    val y = ScalarQuantities(5(m), 2(m))
    val quantities = VectorQuantities(x, y).flatMap(v => VectorQuantities(v, v * 2))

    quantities shouldBe a[VectorQuantities[_, _]]
    quantities.length shouldBe 4
    quantities(0) shouldBe VectorQuantity(3(m), 5(m))
    quantities(1) shouldBe VectorQuantity(6(m), 10(m))
    quantities(2) shouldBe VectorQuantity(2(cm), 2(m))
    quantities(3) shouldBe VectorQuantity(4(cm), 4(m))

  }

  ":+" should "append a new vector quantity" in {

    val x = ScalarQuantities(3(m), 2(cm))
    val y = ScalarQuantities(5(m), 2(m))
    val quantities = VectorQuantities(x, y) :+ VectorQuantity(6(mm), 10(m))

    quantities shouldBe a[VectorQuantities[_, _]]
    quantities.length shouldBe 3
    quantities(0) shouldBe VectorQuantity(3(m), 5(m))
    quantities(1) shouldBe VectorQuantity(2(cm), 2(m))
    quantities(2) shouldBe VectorQuantity(6(mm), 10(m))

  }

  "+:" should "prepend a new vector quantity" in {

    val x = ScalarQuantities(3(m), 2(cm))
    val y = ScalarQuantities(5(m), 2(m))
    val quantities = VectorQuantity(6(mm), 10(m)) +: VectorQuantities(x, y)

    quantities shouldBe a[VectorQuantities[_, _]]
    quantities.length shouldBe 3
    quantities(0) shouldBe VectorQuantity(6(mm), 10(m))
    quantities(1) shouldBe VectorQuantity(3(m), 5(m))
    quantities(2) shouldBe VectorQuantity(2(cm), 2(m))

  }

  "+" should "perform an element wise addition of two lists of vector quantities" in {

    val q1 = VectorQuantities(v1, v2)
    val q2 = VectorQuantities(v3, v4)
    val sum = q1 + q2

    sum shouldBe a[VectorQuantities[_, _]]
    sum.length shouldBe 2
    sum(0) shouldBe (v1 + v3)
    sum(1) shouldBe (v2 + v4)

  }

  it should "add a constant to a list of vector quantities" in {

    val sum = VectorQuantities(v1, v2) + v3

    sum shouldBe a[VectorQuantities[_, _]]
    sum.length shouldBe 2
    sum(0) shouldBe (v1 + v3)
    sum(1) shouldBe (v2 + v3)

  }

  "-" should "perform an element wise subtraction of two lists of vector quantities" in {

    val q1 = VectorQuantities(v1, v2)
    val q2 = VectorQuantities(v3, v4)
    val diff = q1 - q2

    diff shouldBe a[VectorQuantities[_, _]]
    diff.length shouldBe 2
    diff(0) shouldBe (v1 - v3)
    diff(1) shouldBe (v2 - v4)

  }

  it should "subtract a constant to a list of vector quantities" in {

    val sum = VectorQuantities(v1, v2) - v3

    sum shouldBe a[VectorQuantities[_, _]]
    sum.length shouldBe 2
    sum(0) shouldBe (v1 - v3)
    sum(1) shouldBe (v2 - v3)

  }
  
  "-VectorQuantities(2(m), 3(cm))" should "be identical to VectorQuantities(-2(m), -3(cm))" in {
    
    val negate = -VectorQuantities(v1, v2)

    negate shouldBe a[VectorQuantities[_, _]]
    negate.length shouldBe 2
    negate(0) shouldBe -v1
    negate(1) shouldBe -v2
    
  }

  "*" should "perform the element-wise dot product of two lists of vector quantities" in {

    val x = ScalarQuantities(2(m), 5(dm))
    val y = ScalarQuantities(4(m), 1(dm))
    val Fx = ScalarQuantities(5(N), 8(N))
    val Fy = ScalarQuantities(2(N), 4(N))
    val move = VectorQuantities(x, y)
    val force = VectorQuantities(Fx, Fy)
    val work = force * move

    work shouldBe a[ScalarQuantities[_]]
    work.length shouldBe 2
    work(0) shouldBe (2 * 5 + 4 * 2)(J)
    work(1) shouldBe (0.5 * 8 + 0.1 * 4)(J)

  }

  it should "perform the dot product of every vector from a list of quantities by a constant" in {

    val Fx = ScalarQuantities(5(N), 8(N))
    val Fy = ScalarQuantities(2(N), 4(N))
    val force = VectorQuantities(Fx, Fy)
    val work = force * VectorQuantity(2(m), 4(m))

    work shouldBe a[ScalarQuantities[_]]
    work.length shouldBe 2
    work(0) shouldBe (5 * 2 + 2 * 4)(J)
    work(1) shouldBe (8 * 2 + 4 * 4)(J)

  }

  it should "multiply every value of a list of vector quantities by a scalar" in {

    val positions = VectorQuantities(v1, v2) * 2

    positions shouldBe a[VectorQuantities[_, _]]
    positions.length shouldBe 2
    positions(0) shouldBe (v1 * 2)
    positions(1) shouldBe (v2 * 2)

  }

  "/" should "perform the element-wise division of a list of vector quantities by a list of scalar quantities" in {

    val move = VectorQuantities(v1, v2)
    val duration = ScalarQuantities(5(s), 8(s))
    val speed = move / duration

    speed shouldBe a[VectorQuantities[_, _]]
    speed.length shouldBe 2
    speed(0) shouldBe (v1 / 5(s))
    speed(1) shouldBe (v2 / 8(s))

  }

  it should "divide every value of a list of vector quantities by a scalar constant" in {

    val speed = VectorQuantities(v1, v2) / 2(s)

    speed shouldBe a[VectorQuantities[_, _]]
    speed.length shouldBe 2
    speed(0) shouldBe (v1 / 2(s))
    speed(1) shouldBe (v2 / 2(s))

  }

  it should "divide every value of a list of vector quantities by a scalar" in {

    val speed = VectorQuantities(v1, v2) / 2

    speed shouldBe a[VectorQuantities[_, _]]
    speed.length shouldBe 2
    speed(0) shouldBe (v1 / 2)
    speed(1) shouldBe (v2 / 2)

  }

  "VectorQuantities(v1, v2) in cm" should "return 2 vectors expressed in cm" in {

    val length = VectorQuantities(v1, v2) in cm

    length shouldBe a[VectorQuantities[_, _]]
    length.length shouldBe 2
    length(0) shouldBe v1.in(cm)
    length(1) shouldBe v2.in(cm)

  }

  "diff(2)" should "evaluate the difference between vector quantities separated by 2 indexes" in {

    val length = VectorQuantities(v1, v2, v3, v4)
    val step = length.diff(2)

    step shouldBe a[VectorQuantities[_, _]]
    step.length shouldBe 4
    step(0).coordinate(0).isNaN shouldBe true
    step(1).coordinate(0).isNaN shouldBe true
    step(2) shouldBe (v3 - v1)
    step(3) shouldBe (v4 - v2)

  }

  "slice" should "build a subset of vector quantities" in {

    val subset = VectorQuantities(v1, v2, v3, v4).slice(1, 3)

    subset shouldBe a[VectorQuantities[_, _]]
    subset.length shouldBe 2
    subset(0) shouldBe v2
    subset(1) shouldBe v3

  }

}
