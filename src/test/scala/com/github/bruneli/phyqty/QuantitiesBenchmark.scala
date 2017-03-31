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

import Dimension._
import PhyUnit._
import Quantity._

/**
  * @author bruneli
  */
object QuantitiesBenchmark extends App {

  val collectionSizes = List(100, 1000, 100000, 1000000)
  val maxCollectionSize = collectionSizes.max

  for (size <- collectionSizes) {
    val trials = maxCollectionSize * 100 / size
    //vectorAdditionBenchmark(size, trials)
    chainOfOperationsBenchmark(size, trials)
  }

  def vectorAdditionBenchmark(size: Int, trials: Int): Unit = {
    val q1 = Quantities(Array.fill(size)(2.0), metre)
    val q2 = Quantities(Array.fill(size)(3.0), metre)
    val quantitiesMillis = performanceEstimate(quantitiesAddition(q1, q2), trials = trials, warmupTrials = trials)
    println(s"Adding two quantities collections of $size elements took in average $quantitiesMillis milliseconds in average")
    val q3 = Vector.fill(size)(2.0 (m))
    val q4 = Vector.fill(size)(3.0 (m))
    val vectorMillis = performanceEstimate(quantityVectorAddition(q3, q4), trials = trials, warmupTrials = trials)
    println(s"Adding two vectors of quantity of $size elements took in average $vectorMillis milliseconds in average")
    println(s"Quantities are ${math.round(vectorMillis / quantitiesMillis * 100) / 100.0} faster than Vector of Quantity")
  }

  def quantitiesAddition(q1: Quantities[Length], q2: Quantities[Length])(): Unit = {
    q1 + q2
  }

  def quantityVectorAddition(q1: Vector[Quantity[Length]], q2: Vector[Quantity[Length]])(): Unit = {
    (q1, q2).zipped.map(_ + _)
  }

  def chainOfOperationsBenchmark(size: Int, trials: Int): Unit = {
    val position = Quantities(Range(0, size).map(_.toDouble).toArray, metre)
    val time = Quantities(Range(0, size).map(_.toDouble).toArray, second)
    val speed = 1 (m / s)
    val offset = 0 (m)
    val sigma = 20 (cm)
    val quantitiesMillis = performanceEstimate(leastSquaresSum(position, time, speed, offset, sigma), trials = trials,
      warmupTrials = trials)
    println(s"Chaining operations with quantities of $size elements took in average $quantitiesMillis milliseconds in average")
    val quantitiesViewMillis = performanceEstimate(leastSquaresSum(position.view, time.view, speed, offset, sigma),
      trials = trials, warmupTrials = trials)
    println(s"Chaining operations with quantities views of $size elements took in average $quantitiesViewMillis milliseconds in average")
    //val vectorMillis = performanceEstimate(vectorLeastSquaresSum(position.toVector, time.toVector, speed, offset, sigma),
    //  trials = trials, warmupTrials = trials)
    //println(s"Chaining operations with vector of quantities of $size elements took in average $vectorMillis milliseconds in average")
    println(s"QuantitiesView are ${math.round(quantitiesMillis / quantitiesViewMillis * 100) / 100.0} faster than Quantities")
  }

  def leastSquaresSum(position: QuantitiesLike[Length], time: QuantitiesLike[Time], speed: Quantity[Speed],
                      offset: Quantity[Length], sigma: Quantity[Length])(): Unit = {
    val expectedPosition = time * speed + offset
    val residual = (position - expectedPosition) / sigma
    (residual * residual).sum
  }

  def vectorLeastSquaresSum(position: Vector[Quantity[Length]], time: Vector[Quantity[Time]], speed: Quantity[Speed],
                            offset: Quantity[Length], sigma: Quantity[Length])(): Unit = {
    val expectedPosition = time.map(_ * speed + offset)
    val residualSquared = (position, expectedPosition).zipped.map((l, r) => (l - r) * (l - r) / sigma / sigma)
    residualSquared.foldLeft(Quantity(0.0, residualSquared(0).unit)) {
      case (sum, current) => sum + current
    }
  }

  def performanceEstimate(operation: () => Unit, trials: Int = 100, warmupTrials: Int = 100): Double = {

    for (warmUpTrial <- 0 until warmupTrials) {
      operation()
    }

    val start = System.currentTimeMillis()
    for (trial <- 0 until trials) {
      operation()
    }
    val end = System.currentTimeMillis()

    (end - start) / trials.toDouble
  }

}
