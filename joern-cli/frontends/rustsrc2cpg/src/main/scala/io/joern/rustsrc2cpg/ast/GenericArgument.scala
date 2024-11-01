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

class GenericArgument {
  @JsonProperty("lifetime")
  var lifetimeGenericArgument: Option[Lifetime] = None
  @JsonProperty("type")
  var typeGenericArgument: Option[Type] = None
  @JsonProperty("const")
  var constGenericArgument: Option[Expr] = None
  @JsonProperty("assoc_type")
  var assocTypeGenericArgument: Option[AssocType] = None
  @JsonProperty("assoc_const")
  var assocConstGenericArgument: Option[AssocConst] = None
  @JsonProperty("constraint")
  var constraintGenericArgument: Option[Constraint] = None
}

abstract class BaseGenericArgument extends RustAst {
  var ident: Ident                                     = ""
  var generics: Option[AngleBracketedGenericArguments] = None
}

class AssocType extends BaseGenericArgument {
  var ty: Option[Type] = None
}

class AssocConst extends BaseGenericArgument {
  var value: Option[Expr] = None
}

class Constraint extends BaseGenericArgument {
  var bounds: ListBuffer[TypeParamBound] = ListBuffer.empty
}
