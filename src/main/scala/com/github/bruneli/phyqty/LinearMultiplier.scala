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

/**
  * @author bruneli
  */
case class LinearMultiplier(symbol: String, multiplier: Double) extends LinearTransform {

  override def inverseTransform: LinearTransform = LinearMultiplier(s"1/$symbol", 1.0 / multiplier)

  override def value(magnitude: Double): Double = multiplier * magnitude

  override def equals(obj: Any): Boolean = obj match {
    case LinearMultiplier(thatSymbol, thatMultiplier) => this.multiplier == thatMultiplier
    case _ => false
  }

}

object LinearMultiplier {

  val sixtyTimes = LinearMultiplier("", 60.0)

}