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

type TokenStream = ListBuffer[TokenTree]

class TokenTree {
  @JsonProperty("group")
  var group: Option[Group] = None
  @JsonProperty("ident")
  var ident: Option[Ident] = None
  @JsonProperty("punct")
  var punct: Option[Punct] = None
  @JsonProperty("lit")
  var lit: Literal = ""
}

class Group extends RustAst {
  var delimiter: Option[Delimiter] = None
  var stream: Option[TokenStream]  = None
}
class Punct extends RustAst {
  var op: Option[String]       = None
  var spacing: Option[Spacing] = None
}

enum Delimiter(val value: String) extends RustAst {
  case Parenthesis extends Delimiter("parenthesis")
  case Brace       extends Delimiter("brace")
  case Bracket     extends Delimiter("bracket")
  case None        extends Delimiter("")

  @JsonValue
  override def toString: String = value
}

object Delimiter {
  @JsonCreator
  def fromString(value: String): Delimiter = value match {
    case "parenthesis" => Delimiter.Parenthesis
    case "brace"       => Delimiter.Brace
    case "bracket"     => Delimiter.Bracket
    case ""            => Delimiter.None
    case _             => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}

enum Spacing(val value: String) extends RustAst {
  case Alone extends Spacing("alone")
  case Joint extends Spacing("joint")

  @JsonValue
  override def toString: String = value
}

object Spacing {
  @JsonCreator
  def fromString(value: String): Spacing = value match {
    case "alone" => Spacing.Alone
    case "joint" => Spacing.Joint
    case _       => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}
