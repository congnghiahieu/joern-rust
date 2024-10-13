package io.joern.rustsrc2cpg.ast

import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonValue
import io.joern.rustsrc2cpg.ast.*

import scala.collection.mutable.ListBuffer

enum TraitBoundModifier(val value: String) extends RustAst {
  case None  extends TraitBoundModifier("")
  case Maybe extends TraitBoundModifier("maybe")

  @JsonValue
  override def toString: String = value
}

object TraitBoundModifier {
  @JsonCreator
  def fromString(value: String): TraitBoundModifier = value match {
    case ""      => TraitBoundModifier.None
    case "maybe" => TraitBoundModifier.Maybe
    case _       => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}

class TraitBound extends RustAst {
  var paren_token: Option[Boolean]         = None
  var modifier: Option[TraitBoundModifier] = None
  var lifetimes: Option[BoundLifetimes]    = None
  var path: Option[Path]                   = None
}
