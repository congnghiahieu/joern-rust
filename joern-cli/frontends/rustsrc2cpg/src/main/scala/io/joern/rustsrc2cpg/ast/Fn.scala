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

type ReturnType = Option[Type]

trait SignatureEmbeddable {
  private var _const: Option[Boolean]     = None
  private var _async: Option[Boolean]     = None
  private var _unsafe: Option[Boolean]    = None
  private var _abi: Option[Abi]           = None
  private var _ident: Ident               = ""
  private var _generics: Option[Generics] = None
  private var _inputs: ListBuffer[FnArg]  = ListBuffer.empty
  private var _variadic: Option[Variadic] = None
  private var _output: ReturnType         = None

  def const: Option[Boolean] = _const
  @JsonSetter("const")
  def setConst(value: Option[Boolean]): Unit = {
    _const = value
  }

  def async: Option[Boolean] = _async
  @JsonSetter("async")
  def setAsync(value: Option[Boolean]): Unit = {
    _async = value
  }

  def unsafe: Option[Boolean] = _unsafe
  @JsonSetter("unsafe")
  def setUnsafe(value: Option[Boolean]): Unit = {
    _unsafe = value
  }

  def abi: Option[Abi] = _abi
  @JsonSetter("abi")
  def setAbi(value: Option[Abi]): Unit = {
    _abi = value
  }

  def ident: Ident = _ident
  @JsonSetter("ident")
  def setIdent(value: Ident): Unit = {
    _ident = value
  }

  def generics: Option[Generics] = _generics
  @JsonSetter("generics")
  def setGenerics(value: Option[Generics]): Unit = {
    _generics = value
  }

  def inputs: ListBuffer[FnArg] = _inputs
  @JsonSetter("inputs")
  def setInputs(value: ListBuffer[FnArg]): Unit = {
    _inputs = value
  }

  def variadic: Option[Variadic] = _variadic
  @JsonSetter("variadic")
  def setVariadic(value: Option[Variadic]): Unit = {
    _variadic = value
  }

  def output: ReturnType = _output
  @JsonSetter("output")
  def setOutput(value: ReturnType): Unit = {
    _output = value
  }
}

class Signature extends RustAst with SignatureEmbeddable {
  @JsonCreator
  def this(
    @JsonProperty("const") const: Option[Boolean] = None,
    @JsonProperty("async") async: Option[Boolean] = None,
    @JsonProperty("unsafe") unsafe: Option[Boolean] = None,
    @JsonProperty("abi") abi: Option[Abi] = None,
    @JsonProperty("ident") ident: Ident = "",
    @JsonProperty("generics") generics: Option[Generics] = None,
    @JsonProperty("inputs") inputs: ListBuffer[FnArg] = ListBuffer.empty,
    @JsonProperty("variadic") variadic: Option[Variadic] = None,
    @JsonProperty("output") output: ReturnType = None
  ) = {
    this()
    this.setConst(const)
    this.setAsync(async)
    this.setUnsafe(unsafe)
    this.setAbi(abi)
    this.setIdent(ident)
    this.setGenerics(generics)
    this.setInputs(inputs)
    this.setVariadic(variadic)
    this.setOutput(output)
  }
}

class Variadic extends RustAst {
  var attrs: Option[List[Attribute]] = None
  var pat: Option[Pat]               = None
  var comma: Option[Boolean]         = None
}

class Variant extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
  var ident: Ident                         = ""
  var fields: Option[Fields]               = None
  var discriminant: Option[Expr]           = None
}
