package com.github.bruneli.phyqty

/*
 * Copyright 2017 Renaud Bruneliere
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
sealed trait QuantityType {

  def dimension: Int

}

sealed trait Scalar extends QuantityType {

  val dimension = 1

}

sealed trait Vector2D extends QuantityType {

  val dimension = 2

}

sealed trait Vector3D extends QuantityType {

  val dimension = 3

}
