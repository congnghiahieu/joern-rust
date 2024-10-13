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

class WherePredicate {
  @JsonProperty("lifetime")
  var lifetimeWherePredicate: Option[PredicateLifetime] = None
  @JsonProperty("type")
  var typeWherePredicate: Option[PredicateType] = None
}

class PredicateLifetime extends RustAst {
  var lifetime: Lifetime           = ""
  var bounds: ListBuffer[Lifetime] = ListBuffer.empty
}
class PredicateType extends RustAst {
  var lifetimes: Option[BoundLifetimes]  = None
  var bounded_ty: Option[Type]           = None
  var bounds: ListBuffer[TypeParamBound] = ListBuffer.empty
}

type BoundLifetimes = ListBuffer[GenericParam]

type Lifetime = String

type WhereClause = ListBuffer[WherePredicate]
