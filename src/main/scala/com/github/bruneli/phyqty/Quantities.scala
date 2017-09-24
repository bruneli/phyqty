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

import Quantities.QuantitiesDimensionException

import scala.collection.generic.CanBuildFrom
import Dimension.{/, x}

/**
  * @author bruneli
  */
trait Quantities[D <: Dimension[_, _, _, _, _, _, _], N <: QuantityType] extends QuantitiesLike[D, N] {

  type Q[DD <: Dimension[_, _, _, _, _, _, _]] <: Quantities[DD, N]

  override def mapCoordinates(f: Double => Double): Q[D] = {
    val mappedCoordinates = new Array[Double](length * dimension)
    var k = 0
    for {i <- 0 until dimension
         j <- 0 until length} {
      mappedCoordinates(k) = f(coordinate(i, j))
      k += 1
    }
    buildQuantities(mappedCoordinates, unit)
  }

  override def +(that: QuantitiesLike[D, N]): Q[D] = {
    if (this.length == that.length) {
      val uniformize: Double => Double = converter(that.unit)
      val sum = new Array[Double](this.dimension * this.length)
      var k = 0
      for {i <- 0 until dimension
           j <- 0 until length} {
        sum(k) = this.coordinate(i, j) + uniformize(that.coordinate(i, j))
        k += 1
      }
      buildQuantities(sum, unit)
    } else {
      throw new QuantitiesDimensionException
    }
  }

  override def +(that: Quantity[D, N]): Q[D] = {
    val constant = that.in(unit)
    val sum = new Array[Double](this.dimension * this.length)
    var k = 0
    for {i <- 0 until dimension
         j <- 0 until length} {
      sum(k) = this.coordinate(i, j) + constant.coordinate(i)
      k += 1
    }
    buildQuantities(sum, unit)
  }

  override def -(that: QuantitiesLike[D, N]): Q[D] = {
    if (this.length == that.length) {
      val uniformize: Double => Double = converter(that.unit)
      val diff = new Array[Double](this.dimension * this.length)
      var k = 0
      for {i <- 0 until dimension
           j <- 0 until length} {
        diff(k) = this.coordinate(i, j) - uniformize(that.coordinate(i, j))
        k += 1
      }
      buildQuantities(diff, unit)
    } else {
      throw new QuantitiesDimensionException
    }
  }

  override def -(that: Quantity[D, N]): Q[D] = {
    val constant = that.in(unit)
    val diff = new Array[Double](this.dimension * this.length)
    var k = 0
    for {i <- 0 until dimension
         j <- 0 until length} {
      diff(k) = this.coordinate(i, j) - constant.coordinate(i)
      k += 1
    }
    buildQuantities(diff, unit)
  }

  override def unary_-(): Q[D] = {
    val opposite = new Array[Double](this.dimension * this.length)
    var k = 0
    for {i <- 0 until dimension
         j <- 0 until length} {
      opposite(k) = -coordinate(i, j)
      k += 1
    }
    buildQuantities(opposite, unit)
  }

  override def *[DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD, N]): ScalarQuantities[D x DD] = {
    if (this.length == that.length) {
      val prod = Array.fill(this.length)(0.0)
      for {i <- 0 until dimension
           j <- 0 until length} {
        prod(j) += this.coordinate(i, j) * that.coordinate(i, j)
      }
      ScalarQuantities(prod, this.unit * that.unit)
    } else {
      throw new QuantitiesDimensionException
    }
  }

  override def *[DD <: Dimension[_, _, _, _, _, _, _]](that: Quantity[DD, N]): ScalarQuantities[D x DD] = {
    val prod = Array.fill(this.length)(0.0)
    for {i <- 0 until dimension
         j <- 0 until length} {
      prod(j) += this.coordinate(i, j) * that.coordinate(i)
    }
    ScalarQuantities(prod, this.unit * that.unit)
  }

  override def *(scalar: Double): Q[D] = {
    val prod = new Array[Double](this.dimension * this.length)
    var k = 0
    for {i <- 0 until dimension
         j <- 0 until length} {
      prod(k) = coordinate(i, j) * scalar
      k += 1
    }
    buildQuantities(prod, unit)
  }

  override def /[DD <: Dimension[_, _, _, _, _, _, _]](that: QuantitiesLike[DD, Scalar]): Q[D / DD] = {
    if (this.length == that.length) {
      val ratio = new Array[Double](this.dimension * this.length)
      var k = 0
      for {i <- 0 until dimension
           j <- 0 until length} {
        ratio(k) = this.coordinate(i, j) / that.magnitude(j)
        k += 1
      }
      buildQuantities(ratio, this.unit / that.unit)
    } else {
      throw new QuantitiesDimensionException
    }
  }

  override def /[DD <: Dimension[_, _, _, _, _, _, _]](that: ScalarQuantity[DD]): Q[D / DD] = {
    val ratio = new Array[Double](this.dimension * this.length)
    var k = 0
    for {i <- 0 until dimension
         j <- 0 until length} {
      ratio(k) = this.coordinate(i, j) / that.magnitude
      k += 1
    }
    buildQuantities(ratio, this.unit / that.unit)
  }

  override def /(scalar: Double): Q[D] = {
    val ratio = new Array[Double](this.dimension * this.length)
    var k = 0
    for {i <- 0 until dimension
         j <- 0 until length} {
      ratio(k) = this.coordinate(i, j) / scalar
      k += 1
    }
    buildQuantities(ratio, unit)
  }

  override def in(anotherUnit: PhyUnit[D]): Q[D] = {
    val converted = new Array[Double](this.dimension * this.length)
    var k = 0
    for {i <- 0 until dimension
         j <- 0 until length} {
      converted(k) = anotherUnit.converter.inverse(this.coordinate(i, j), unit.converter)
      k += 1
    }
    buildQuantities(converted, anotherUnit)
  }

  protected def buildQuantities[DD <: Dimension[_, _, _, _, _, _, _]](coordinates: Array[Double], unit: PhyUnit[DD]): Q[DD]

}

object Quantities {

  class QuantitiesDimensionException extends RuntimeException("quantities vectors must be equal in size")

}
