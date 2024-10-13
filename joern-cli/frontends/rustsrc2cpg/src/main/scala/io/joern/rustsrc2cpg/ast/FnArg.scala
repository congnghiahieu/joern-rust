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

class FnArg {
  @JsonProperty("receiver")
  var receiverFnArg: Option[Receiver] = None
  @JsonProperty("typed")
  var typedFnArg: Option[PatType] = None
}

class Receiver extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
  var ref: Option[Boolean]                 = None
  var lifetime: Option[Lifetime]           = None
  var mut: Option[Boolean]                 = None
  var colon_token: Option[Boolean]         = None
  var ty: Option[Type]                     = None
}

class BareFnArg extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
  var name: Option[String]                 = None
  var ty: Option[Type]                     = None
}

class BareVariadic extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
  var name: Option[String]                 = None
  var comma: Option[Boolean]               = None
}
