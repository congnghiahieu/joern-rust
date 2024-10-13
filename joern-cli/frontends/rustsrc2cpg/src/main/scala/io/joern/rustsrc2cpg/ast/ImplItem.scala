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

class ImplItem {
  @JsonProperty("const")
  var constImplItem: Option[ImplItemConst] = None
  @JsonProperty("fn")
  var fnImplItem: Option[ImplItemFn] = None
  @JsonProperty("type")
  var typeImplItem: Option[ImplItemType] = None
  @JsonProperty("macro")
  var macroImplItem: Option[ImplItemMacro] = None
  @JsonProperty("verbatim")
  var verbatimImplItem: Option[TokenStream] = None
}

class BaseImplItem extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
}

class ImplItemConst extends BaseImplItem {
  var vis: Option[Visibility]    = None
  var default: Option[Boolean]   = None
  var ident: Ident               = ""
  var generics: Option[Generics] = None
  var ty: Option[Type]           = None
  var expr: Option[Expr]         = None
}

class ImplItemFn extends BaseImplItem with SignatureEmbeddable {
  var vis: Option[Visibility]  = None
  var default: Option[Boolean] = None
  // @JsonUnwrapped
  // var sig: Option[Signature] = None
  var stmts: Block = ListBuffer.empty
}

class ImplItemType extends BaseImplItem {
  var vis: Option[Visibility]    = None
  var default: Option[Boolean]   = None
  var ident: Ident               = ""
  var generics: Option[Generics] = None
  var ty: Option[Type]           = None
}

class ImplItemMacro extends BaseImplItem with MacroEmbeddable {
  // @JsonProperty("macro")
  // var mac: Option[Macro]          = None
  var semi_token: Option[Boolean] = None
}

class QSelf extends RustAst {
  var ty: Option[Type]          = None
  var position: Int             = 0
  var as_token: Option[Boolean] = None
}
