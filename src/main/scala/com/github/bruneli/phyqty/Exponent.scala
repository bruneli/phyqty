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
sealed trait Exponent {

  type NEG <: Exponent

  type PLUS[E <: Exponent] <: Exponent

  type MINUS[E <: Exponent] <: Exponent

  type PREV <: Exponent

  type NEXT <: Exponent
  
}

sealed trait ZeroExponent extends Exponent {

  type NEG = Exponent.Zero

  type PLUS[E <: Exponent] = E

  type MINUS[E <: Exponent] = E#NEG

  type PREV = Exponent.MinusOne

  type NEXT = Exponent.One
  
}

sealed trait PositiveExponent[O <: Exponent, P <: Exponent, N <: Exponent] extends Exponent {

  type NEG = O

  type PLUS[E <: Exponent] = PREV#PLUS[E#NEXT]

  type MINUS[E <: Exponent] = PREV#PLUS[E#NEG#NEXT]

  type PREV = P

  type NEXT = N

}

sealed trait NegativeExponent[O <: Exponent, P <: Exponent, N <: Exponent] extends Exponent {

  type NEG = O

  type PLUS[E <: Exponent] = NEXT#PLUS[E#PREV]

  type MINUS[E <: Exponent] = NEXT#PLUS[E#NEG#PREV]

  type PREV = P

  type NEXT = N

}

object Exponent {

  type +[L <: Exponent, R <: Exponent] = L#PLUS[R]
  type -[L <: Exponent, R <: Exponent] = L#MINUS[R]

  // Limited to +- seven exponents for sbt compilation reasons (stackoverflow)
  sealed trait MinusSeven extends NegativeExponent[Seven, MinusSeven, MinusSix]
  sealed trait MinusSix extends NegativeExponent[Six, MinusSeven, MinusFive]
  sealed trait MinusFive extends NegativeExponent[Five, MinusSix, MinusFour]
  sealed trait MinusFour extends NegativeExponent[Four, MinusFive, MinusThree]
  sealed trait MinusThree extends NegativeExponent[Three, MinusFour, MinusTwo]
  sealed trait MinusTwo extends NegativeExponent[Two, MinusThree, MinusOne]
  sealed trait MinusOne extends NegativeExponent[One, MinusTwo, Zero]
  sealed trait Zero extends ZeroExponent
  sealed trait One extends PositiveExponent[MinusOne, Zero, Two]
  sealed trait Two extends PositiveExponent[MinusTwo, One, Three]
  sealed trait Three extends PositiveExponent[MinusThree, Two, Four]
  sealed trait Four extends PositiveExponent[MinusFour, Three, Five]
  sealed trait Five extends PositiveExponent[MinusFive, Four, Six]
  sealed trait Six extends PositiveExponent[MinusSix, Five, Seven]
  sealed trait Seven extends PositiveExponent[MinusSeven, Six, Seven]

  case class ExponentToInt[E <: Exponent](exponent: Int) extends AnyVal
  implicit val minusSeven = ExponentToInt[MinusSeven](-7)
  implicit val minusSix = ExponentToInt[MinusSix](-6)
  implicit val minusFive = ExponentToInt[MinusFive](-5)
  implicit val minusFour = ExponentToInt[MinusFour](-4)
  implicit val minusThree = ExponentToInt[MinusThree](-3)
  implicit val minusTwo = ExponentToInt[MinusTwo](-2)
  implicit val minusOne = ExponentToInt[MinusOne](-1)
  implicit val zero = ExponentToInt[Zero](0)
  implicit val one = ExponentToInt[One](1)
  implicit val two = ExponentToInt[Two](2)
  implicit val three = ExponentToInt[Three](3)
  implicit val four = ExponentToInt[Four](4)
  implicit val five = ExponentToInt[Five](5)
  implicit val six = ExponentToInt[Six](6)
  implicit val seven = ExponentToInt[Seven](7)

  def exponent[E <: Exponent](implicit exponentToInt: ExponentToInt[E]): Int = {
    exponentToInt.exponent
  }

}






