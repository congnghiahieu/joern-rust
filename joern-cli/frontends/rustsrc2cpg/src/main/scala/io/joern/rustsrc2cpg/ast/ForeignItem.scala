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

class ForeignItem {
  @JsonProperty("fn")
  var fnForeignItem: Option[ForeignItemFn] = None
  @JsonProperty("static")
  var staticForeignItem: Option[ForeignItemStatic] = None
  @JsonProperty("type")
  var typeForeignItem: Option[ForeignItemType] = None
  @JsonProperty("macro")
  var macroForeignItem: Option[ForeignItemMacro] = None
  @JsonProperty("verbatim")
  var verbatimForeignItem: Option[TokenStream] = None
}

abstract class BaseForeignItem extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
}

class ForeignItemFn extends BaseForeignItem with SignatureEmbeddable {
  var vis: Option[Visibility] = None
  // @JsonUnwrapped
  // var sig: Option[Signature] = None
}

class ForeignItemStatic extends BaseForeignItem {
  var vis: Option[Visibility]       = None
  var mut: Option[StaticMutability] = None
  var ident: Ident                  = ""
  var ty: Option[Type]              = None
}

class ForeignItemType extends BaseForeignItem {
  var vis: Option[Visibility]    = None
  var ident: Ident               = ""
  var generics: Option[Generics] = None
}

class ForeignItemMacro extends BaseForeignItem with MacroEmbeddable {
  // @JsonProperty("macro")
  // var mac: Option[Macro]          = None
  var semi_token: Option[Boolean] = None
}
