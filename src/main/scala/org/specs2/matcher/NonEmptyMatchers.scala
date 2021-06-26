package org.specs2.matcher

import cats.data.*
import org.specs2.collection.*

trait NonEmptyMatchers:

  given [T]: Sized[NonEmptyList[T]] with
    def size(t: NonEmptyList[T]): Int = t.length

  given [T]: Sized[NonEmptyChain[T]] with
    def size(t: NonEmptyChain[T]): Int = t.length.toInt

  given [T]: Sized[NonEmptyVector[T]] with
    def size(t: NonEmptyVector[T]): Int = t.length
