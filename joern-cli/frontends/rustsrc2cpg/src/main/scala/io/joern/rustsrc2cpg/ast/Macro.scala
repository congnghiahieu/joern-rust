package io.joern.rustsrc2cpg.ast

import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.JsonSubTypes
import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.fasterxml.jackson.annotation.JsonTypeName
import com.fasterxml.jackson.annotation.JsonUnwrapped
import com.fasterxml.jackson.annotation.JsonValue
import io.joern.rustsrc2cpg.ast.*

import scala.collection.mutable.ListBuffer

enum MacroDelimiter(val value: String) extends RustAst {
  case Paren   extends MacroDelimiter("paren")
  case Brace   extends MacroDelimiter("brace")
  case Bracket extends MacroDelimiter("bracket")

  @JsonValue
  override def toString: String = value
}

object MacroDelimiter {
  @JsonCreator
  def fromString(value: String): MacroDelimiter = value match {
    case "paren"   => MacroDelimiter.Paren
    case "brace"   => MacroDelimiter.Brace
    case "bracket" => MacroDelimiter.Bracket
    case _         => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}

trait MacroEmbeddable {
  private var _path: Option[Path]                = None
  private var _delimiter: Option[MacroDelimiter] = None
  private var _tokens: Option[TokenStream]       = None

  def path: Option[Path] = _path
  @JsonSetter("path")
  def setPath(value: Option[Path]): Unit = {
    _path = value
  }

  def delimiter: Option[MacroDelimiter] = _delimiter
  @JsonSetter("delimiter")
  def setDelimiter(value: Option[MacroDelimiter]): Unit = {
    _delimiter = value
  }

  def tokens: Option[TokenStream] = _tokens
  @JsonSetter("tokens")
  def setTokens(value: Option[TokenStream]): Unit = {
    _tokens = value
  }
}

class Macro extends RustAst with MacroEmbeddable {
  @JsonCreator
  def this(
    @JsonProperty("path") path: Option[Path] = None,
    @JsonProperty("delimiter") delimiter: Option[MacroDelimiter] = None,
    @JsonProperty("tokens") tokens: Option[TokenStream] = None
  ) = {
    this()
    this.setPath(path)
    this.setDelimiter(delimiter)
    this.setTokens(tokens)
  }
}
