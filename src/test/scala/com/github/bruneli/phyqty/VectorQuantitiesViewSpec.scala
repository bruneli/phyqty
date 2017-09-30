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

import com.github.bruneli.phyqty.PhyUnit._
import com.github.bruneli.phyqty.ScalarQuantity._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.SeqView

/**
  * @author bruneli
  */
class VectorQuantitiesViewSpec extends FlatSpec with Matchers {

  val v1 = VectorQuantity(2(m), 4(m))
  val v2 = VectorQuantity(7(m), 14(m))
  val v3 = VectorQuantity(15(cm), 30(cm))
  val v4 = VectorQuantity(1(m), 0.5(m))

  "+" should "perform an element wise addition of two list of vector quantities" in {

    val q1 = VectorQuantities(v1, v2)
    val q2 = VectorQuantities(v3, v4)
    val sum = q1.view + q2

    sum shouldBe a[VectorQuantitiesView[_, _, _]]
    sum.length shouldBe 2
    sum(0) shouldBe (v1 + v3)
    sum(1) shouldBe (v2 + v4)

  }

  it should "add a constant to a list of vector quantities" in {

    val sum = VectorQuantities(v1, v2).view + v3

    sum shouldBe a[VectorQuantitiesView[_, _, _]]
    sum.length shouldBe 2
    sum(0) shouldBe (v1 + v3)
    sum(1) shouldBe (v2 + v3)

  }

  "-" should "perform an element wise subtraction of two list of vector quantities" in {

    val q1 = VectorQuantities(v1, v2)
    val q2 = VectorQuantities(v3, v4)
    val diff = q1.view - q2

    diff shouldBe a[VectorQuantitiesView[_, _, _]]
    diff.length shouldBe 2
    diff(0) shouldBe (v1 - v3)
    diff(1) shouldBe (v2 - v4)

  }

  it should "subtract a constant to a list of scalar quantities" in {

    val diff = VectorQuantities(v1, v2).view - v3

    diff shouldBe a[VectorQuantitiesView[_, _, _]]
    diff.length shouldBe 2
    diff(0) shouldBe (v1 - v3)
    diff(1) shouldBe (v2 - v3)

  }

  "-VectorQuantities(2(m), 3(cm))" should "be identical to vector quantities(-2(m), -3(cm))" in {

    val length = VectorQuantities(v1, v2).view
    val negate = -length

    negate shouldBe a[VectorQuantitiesView[_, _, _]]
    negate.length shouldBe 2
    negate(0) shouldBe -v1
    negate(1) shouldBe -v2

  }

  "*" should "perform the element-wise product of two list of vector quantities" in {

    val x = ScalarQuantities(2(m), 5(dm))
    val y = ScalarQuantities(4(m), 1(dm))
    val Fx = ScalarQuantities(5(N), 8(N))
    val Fy = ScalarQuantities(2(N), 4(N))
    val move = VectorQuantities(x, y)
    val force = VectorQuantities(Fx, Fy)
    val work = force.view * move

    work shouldBe a[ScalarQuantitiesView[_, _]]
    work.length shouldBe 2
    work(0) shouldBe (2 * 5 + 4 * 2)(J)
    work(1) shouldBe (0.5 * 8 + 0.1 * 4)(J)

  }

  it should "multiply every value of a list of vector quantities by a constant" in {

    val Fx = ScalarQuantities(5(N), 8(N))
    val Fy = ScalarQuantities(2(N), 4(N))
    val force = VectorQuantities(Fx, Fy)
    val work = force.view * VectorQuantity(2(m), 4(m))

    work shouldBe a[ScalarQuantitiesView[_, _]]
    work.length shouldBe 2
    work(0) shouldBe (5 * 2 + 2 * 4)(J)
    work(1) shouldBe (8 * 2 + 4 * 4)(J)

  }

  it should "multiply every value of a list of vector quantities by a scalar" in {

    val positions = VectorQuantities(v1, v2).view * 2

    positions shouldBe a[VectorQuantitiesView[_, _, _]]
    positions.length shouldBe 2
    positions(0) shouldBe (v1 * 2)
    positions(1) shouldBe (v2 * 2)

  }

  "/" should "perform the element-wise division of two list of vector quantities" in {

    val move = VectorQuantities(v1, v2)
    val duration = ScalarQuantities(5(s), 8(s))
    val speed = move.view / duration

    speed shouldBe a[VectorQuantitiesView[_, _, _]]
    speed.length shouldBe 2
    speed(0) shouldBe (v1 / 5(s))
    speed(1) shouldBe (v2 / 8(s))

  }

  it should "divide every value of a list of vector quantities by a constant" in {

    val speed = VectorQuantities(v1, v2).view / 2(s)

    speed shouldBe a[VectorQuantitiesView[_, _, _]]
    speed.length shouldBe 2
    speed(0) shouldBe (v1 / 2(s))
    speed(1) shouldBe (v2 / 2(s))

  }

  it should "divide every value of a list of vector quantities by a scalar" in {

    val speed = VectorQuantities(v1, v2).view / 2

    speed shouldBe a[VectorQuantitiesView[_, _, _]]
    speed.length shouldBe 2
    speed(0) shouldBe (v1 / 2)
    speed(1) shouldBe (v2 / 2)

  }

  "VectorQuantitiesView(3(m), 6(m)) in cm" should "be VectorQuantitiesView(300(cm), 600(cm))" in {

    val length = VectorQuantities(v1, v2).view in cm

    length shouldBe a[VectorQuantitiesView[_, _, _]]
    length.length shouldBe 2
    length(0) shouldBe v1.in(cm)
    length(1) shouldBe v2.in(cm)

  }

  "slice" should "build a subset of vector quantities" in {

    val subset = VectorQuantities(v1, v2, v3, v4).view.slice(1, 3)

    subset shouldBe a[VectorQuantitiesView[_, _, _]]
    subset.length shouldBe 2
    subset(0) shouldBe v2
    subset(1) shouldBe v3

  }

  "map" should "transform VectorQuantities into a view sequence" in {

    val length = VectorQuantities(v1, v2, v3).view
    val deltaT = 2(s)
    val speed = length.map(_ / deltaT)

    speed shouldBe a[SeqView[_, _]]
    speed.length shouldBe 3
    speed(0) shouldBe (v1 / deltaT)
    speed(1) shouldBe (v2 / deltaT)
    speed(2) shouldBe (v3 / deltaT)

  }

}
