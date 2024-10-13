package io.joern.rustsrc2cpg.ast

import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSubTypes
import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.fasterxml.jackson.annotation.JsonTypeName
import com.fasterxml.jackson.annotation.JsonUnwrapped
import com.fasterxml.jackson.annotation.JsonValue
import io.joern.rustsrc2cpg.ast.*

import scala.collection.mutable.ListBuffer

enum AttrStyle(val value: String) extends RustAst {
  case Inner extends AttrStyle("inner")
  case Outer extends AttrStyle("outer")

  @JsonValue
  override def toString: String = value
}

object AttrStyle {
  @JsonCreator
  def fromString(value: String): AttrStyle = value match {
    case "inner" => AttrStyle.Inner
    case "outer" => AttrStyle.Outer
    case _       => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}

class Attribute extends RustAst {
  var style: Option[AttrStyle] = None
  var meta: Option[Meta]       = None
}
