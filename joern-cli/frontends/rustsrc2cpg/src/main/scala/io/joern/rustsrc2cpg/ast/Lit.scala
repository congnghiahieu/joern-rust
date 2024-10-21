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

trait LitEmbeddable {
  private var _strLit: Option[LitStr]         = None
  private var _byteStrLit: Option[LitByteStr] = None
  private var _byteLit: Option[LitByte]       = None
  private var _charLit: Option[LitChar]       = None
  private var _intLit: Option[LitInt]         = None
  private var _floatLit: Option[LitFloat]     = None
  private var _boolLit: Option[LitBool]       = None
  private var _verbatimLit: Option[Literal]   = None

  def strLit: Option[LitStr] = _strLit
  @JsonSetter("str")
  def setStrLit(value: Option[LitStr]): Unit = {
    _strLit = value
  }

  def byteStrLit: Option[LitByteStr] = _byteStrLit
  @JsonSetter("byte_str")
  def setByteStrLit(value: Option[LitByteStr]): Unit = {
    _byteStrLit = value
  }

  def byteLit: Option[LitByte] = _byteLit
  @JsonSetter("byte")
  def setByteLit(value: Option[LitByte]): Unit = {
    _byteLit = value
  }

  def charLit: Option[LitChar] = _charLit
  @JsonSetter("char")
  def setCharLit(value: Option[LitChar]): Unit = {
    _charLit = value
  }

  def intLit: Option[LitInt] = _intLit
  @JsonSetter("int")
  def setIntLit(value: Option[LitInt]): Unit = {
    _intLit = value
  }

  def floatLit: Option[LitFloat] = _floatLit
  @JsonSetter("float")
  def setFloatLit(value: Option[LitFloat]): Unit = {
    _floatLit = value
  }

  def boolLit: Option[LitBool] = _boolLit
  @JsonSetter("bool")
  def setBoolLit(value: Option[LitBool]): Unit = {
    _boolLit = value
  }

  def verbatimLit: Option[Literal] = _verbatimLit
  @JsonSetter("verbatim")
  def setVerbatimLit(value: Option[Literal]): Unit = {
    _verbatimLit = value
  }
}

class Lit extends RustAst with LitEmbeddable {
  @JsonCreator
  def this(
    @JsonProperty("str") strLit: Option[LitStr] = None,
    @JsonProperty("byte_str") byteStrLit: Option[LitByteStr] = None,
    @JsonProperty("byte") byteLit: Option[LitByte] = None,
    @JsonProperty("char") charLit: Option[LitChar] = None,
    @JsonProperty("int") intLit: Option[LitInt] = None,
    @JsonProperty("float") floatLit: Option[LitFloat] = None,
    @JsonProperty("bool") boolLit: Option[LitBool] = None,
    @JsonProperty("verbatim") verbatimLit: Option[Literal] = None
  ) = {
    this()
    setStrLit(strLit)
    setByteStrLit(byteStrLit)
    setByteLit(byteLit)
    setCharLit(charLit)
    setIntLit(intLit)
    setFloatLit(floatLit)
    setBoolLit(boolLit)
    setVerbatimLit(verbatimLit)
  }
}

type LitStr     = String
type LitByteStr = String
type LitByte    = String
type LitChar    = String
type LitInt     = String
type LitFloat   = String
type LitBool    = String
type Literal    = String;
