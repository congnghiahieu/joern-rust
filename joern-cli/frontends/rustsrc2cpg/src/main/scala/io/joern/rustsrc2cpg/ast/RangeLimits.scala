package io.joern.rustsrc2cpg.ast

import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonValue
import io.joern.rustsrc2cpg.ast.*

enum RangeLimits(val value: String) extends RustAst {
  case HalfOpen extends RangeLimits("..")
  case Closed   extends RangeLimits("..=")

  @JsonValue
  override def toString: String = value
}

object RangeLimits {
  @JsonCreator
  def fromString(value: String): RangeLimits = value match {
    case ".."  => RangeLimits.HalfOpen
    case "..=" => RangeLimits.Closed
    case _     => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}
