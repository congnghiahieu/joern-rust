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

class GenericParam {
  @JsonProperty("lifetime")
  var lifetimeGenericParam: Option[LifetimeParam] = None
  @JsonProperty("type")
  var typeGenericParam: Option[TypeParam] = None
  @JsonProperty("const")
  var constGenericParam: Option[ConstParam] = None
}

abstract class BaseGenericParam extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
}

class LifetimeParam extends BaseGenericParam {
  var lifetime: Lifetime           = ""
  var colon_token: Option[Boolean] = None
  var bounds: ListBuffer[Lifetime] = ListBuffer.empty
}

class TypeParam extends BaseGenericParam {
  var ident: Ident                       = ""
  var colon_token: Option[Boolean]       = None
  var bounds: ListBuffer[TypeParamBound] = ListBuffer.empty
  var eq_token: Option[Boolean]          = None
  var default: Option[Type]              = None
}

class ConstParam extends BaseGenericParam {
  var ident: Ident              = ""
  var ty: Option[Type]          = None
  var eq_token: Option[Boolean] = None
  var default: Option[Expr]     = None
}
