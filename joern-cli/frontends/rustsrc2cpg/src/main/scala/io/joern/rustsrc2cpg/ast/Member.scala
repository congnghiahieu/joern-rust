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

trait MemberEmbeddable {
  private var _named: Option[Ident]   = None
  private var _unnamed: Option[Index] = None

  def named: Option[Ident] = _named
  @JsonSetter("ident")
  def setNamed(value: Option[Ident]): Unit = {
    _named = value
  }

  def unnamed: Option[Index] = _unnamed
  @JsonSetter("index")
  def setUnnamed(value: Option[Index]): Unit = {
    _unnamed = value
  }
}

class Member extends RustAst with MemberEmbeddable {
  @JsonCreator
  def this(
    @JsonProperty("ident") named: Option[Ident] = None,
    @JsonProperty("index") unnamed: Option[Index] = None
  ) = {
    this()
    this.setNamed(named)
    this.setUnnamed(unnamed)
  }
}

type Ident = String
type Index = Int
