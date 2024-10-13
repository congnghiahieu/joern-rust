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

class TypeParamBound {
  @JsonProperty("trait")
  var traitParamBound: Option[TraitBound] = None
  @JsonProperty("lifetime")
  var lifetimeParamBound: Option[Lifetime] = None
  @JsonProperty("verbatim")
  var verbatimParamBound: Option[TokenStream] = None
}
