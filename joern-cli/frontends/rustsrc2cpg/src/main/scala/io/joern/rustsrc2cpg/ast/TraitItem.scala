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

class TraitItem {
  @JsonProperty("const")
  var constTraitItem: Option[TraitItemConst] = None
  @JsonProperty("fn")
  var fnTraitItem: Option[TraitItemFn] = None
  @JsonProperty("type")
  var typeTraitItem: Option[TraitItemType] = None
  @JsonProperty("macro")
  var macroTraitItem: Option[TraitItemMacro] = None
  @JsonProperty("verbatim")
  var verbatimTraitItem: Option[TokenStream] = None
}

abstract class BaseTraitItem extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
}

class TraitItemConst extends BaseTraitItem {
  var ident: Ident               = ""
  var generics: Option[Generics] = None
  var ty: Option[Type]           = None
  var default: Option[Expr]      = None
}

class TraitItemFn extends BaseTraitItem with SignatureEmbeddable {
  // @JsonUnwrapped
  // var sig: Option[Signature] = None
  var default: Option[Block] = None
}

class TraitItemType extends BaseTraitItem {
  var ident: Ident                       = ""
  var generics: Option[Generics]         = None
  var colon_token: Option[Boolean]       = None
  var bounds: ListBuffer[TypeParamBound] = ListBuffer.empty
  var default: Option[Type]              = None
}

class TraitItemMacro extends BaseTraitItem with MacroEmbeddable {
  // @JsonProperty("macro")
  // var mac: Option[Macro]          = None
  var semi_token: Option[Boolean] = None
}
