package com.github.bruneli.phyqty

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author bruneli
  */
class VectorQuantitySpec extends FlatSpec with Matchers {

  import PhyUnit._
  import ScalarQuantity._

  "apply" should "return a scalar quantity" in {

    val position = VectorQuantity(-4 (m), 2.5 (m), 5 (cm))

    position(0) shouldBe a[ScalarQuantity[_]]
    position(0) shouldBe -4(m)

  }

  "take(2)" should "return a new vector quantity with 2 coordinates" in {

    val force = VectorQuantity(2(N), -3(N), 4(N))
    val projection = force.take(2)

    projection shouldBe a[VectorQuantity[_, _]]
    projection.length shouldBe 2
    projection(0) shouldBe 2(N)
    projection(1) shouldBe -3(N)

  }

  "v1 + v2" should "return the sum of two vector quantities" in {

    val v1 = VectorQuantity(-4 (m), 2.5 (m), 5 (m))
    val v2 = VectorQuantity(4 (m), 1.5 (m), -2 (m))
    val v3 = v1 + v2

    v3 shouldBe a[VectorQuantity[_, _]]
    v3.length shouldBe 3
    v3(0) shouldBe 0(m)
    v3(1) shouldBe 4(m)
    v3(2) shouldBe 3(m)

  }

  "v1 - v2" should "return the difference of two vector quantities" in {

    val v1 = VectorQuantity(-4 (m), 2.5 (m), 5 (m))
    val v2 = VectorQuantity(4 (m), 1.5 (m), -2 (m))
    val v3 = v1 - v2

    v3 shouldBe a[VectorQuantity[_, _]]
    v3.length shouldBe 3
    v3(0) shouldBe -8(m)
    v3(1) shouldBe 1(m)
    v3(2) shouldBe 7(m)

  }

  "-v" should "return the opposite of a vector quantity" in {

    val force = VectorQuantity(2(N), -3(N), 4(N))
    val opposite = -force

    opposite shouldBe a[VectorQuantity[_, _]]
    opposite.length shouldBe 3
    opposite(0) shouldBe -2(N)
    opposite(1) shouldBe 3(N)
    opposite(2) shouldBe -4(N)

  }

  "v * 2" should "return the vector quantity scaled by 2" in {

    val position = VectorQuantity(-4 (m), 2.5 (m), 5 (cm))
    val scaled = position * 2.0

    scaled shouldBe a[VectorQuantity[_, _]]
    scaled.length shouldBe 3
    scaled(0) shouldBe -8(m)
    scaled(1) shouldBe 5(m)
    scaled(2) shouldBe 0.1(m)

  }

  "position + speed * 2(s)" should "return a position vector" in {

    val x0 = VectorQuantity(-4(m), 2(m), 5(m))
    val v = VectorQuantity(2(m/s), 3(m/s), 1(m/s))
    val x1 = v * 2(s) + x0

    x1 shouldBe a[VectorQuantity[_, _]]
    x1.length shouldBe 3
    x1(0) shouldBe 0(m)
    x1(1) shouldBe 8(m)
    x1(2) shouldBe 7(m)

  }

  "v / 2" should "return the vector quantity scaled by 0.5" in {

    val position = VectorQuantity(-4 (m), 2.5 (m), 5 (cm))
    val scaled = position / 2.0

    scaled shouldBe a[VectorQuantity[_, _]]
    scaled.length shouldBe 3
    scaled(0) shouldBe -2(m)
    scaled(1) shouldBe 1.25(m)
    scaled(2) shouldBe 2.5(cm)

  }

  "distance / 2(s)" should "return a speed vector" in {

    val distance = VectorQuantity(-4(m), 2(m), 5(m))
    val speed = distance / 2(s)

    speed shouldBe a[VectorQuantity[_, _]]
    speed.length shouldBe 3
    speed(0) shouldBe -2(m/s)
    speed(1) shouldBe 1(m/s)
    speed(2) shouldBe 2.5(m/s)

  }

  "force * distance" should "return a scalar work" in {

    val force = VectorQuantity(2(N), -3(N), 4(N))
    val distance = VectorQuantity(-4(m), 2(m), 5(m))
    val work = force * distance

    work shouldBe a[ScalarQuantity[_]]
    work shouldBe 6(J)

  }

}
