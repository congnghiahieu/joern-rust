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

class Item {
  @JsonProperty("const")
  var constItem: Option[ItemConst] = None
  @JsonProperty("enum")
  var enumItem: Option[ItemEnum] = None
  @JsonProperty("extern_crate")
  var externCrateItem: Option[ItemExternCrate] = None
  @JsonProperty("fn")
  var fnItem: Option[ItemFn] = None
  @JsonProperty("foreign_mod")
  var foreignModItem: Option[ItemForeignMod] = None
  @JsonProperty("impl")
  var implItem: Option[ItemImpl] = None
  @JsonProperty("macro")
  var macroItem: Option[ItemMacro] = None
  @JsonProperty("mod")
  var modItem: Option[ItemMod] = None
  @JsonProperty("static")
  var staticItem: Option[ItemStatic] = None
  @JsonProperty("struct")
  var structItem: Option[ItemStruct] = None
  @JsonProperty("trait")
  var traitItem: Option[ItemTrait] = None
  @JsonProperty("trait_alias")
  var traitAliasItem: Option[ItemTraitAlias] = None
  @JsonProperty("type")
  var typeItem: Option[ItemType] = None
  @JsonProperty("union")
  var unionItem: Option[ItemUnion] = None
  @JsonProperty("use")
  var useItem: Option[ItemUse] = None
  @JsonProperty("verbatim")
  var verbatimItem: Option[TokenStream] = None
}

abstract class BaseItem extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
}

class ItemConst extends BaseItem {
  var vis: Option[Visibility]    = None
  var default: Option[Boolean]   = None
  var ident: Ident               = ""
  var generics: Option[Generics] = None
  var ty: Option[Type]           = None
  var expr: Option[Expr]         = None
}

class ItemEnum extends BaseItem {
  var vis: Option[Visibility]       = None
  var ident: Ident                  = ""
  var generics: Option[Generics]    = None
  var variants: ListBuffer[Variant] = ListBuffer.empty
}

class ItemExternCrate extends BaseItem {
  var vis: Option[Visibility] = None
  var ident: Ident            = ""
  var rename: Option[Ident]   = None
}

class ItemFn extends BaseItem with SignatureEmbeddable {
  var vis: Option[Visibility] = None
  var stmts: Block            = ListBuffer.empty
  // @JsonUnwrapped
  // var sig: Option[Signature] = None
}

class ItemForeignMod extends BaseItem {
  var unsafe: Option[Boolean]        = None
  var abi: Option[Abi]               = None
  var items: ListBuffer[ForeignItem] = ListBuffer.empty
}

class ItemImpl extends BaseItem {
  var default: Option[Boolean]   = None
  var unsafe: Option[Boolean]    = None
  var generics: Option[Generics] = None
  @JsonProperty("trait")
  var traitImpl: Option[(Boolean, Path)] = None
  var self_ty: Option[Type]              = None
  var items: ListBuffer[ImplItem]        = ListBuffer.empty
}

class ItemMacro extends BaseItem with MacroEmbeddable {
  var ident: Option[Ident] = None
  // @JsonProperty("macro")
  // var mac: Option[Macro]          = None
  var semi_token: Option[Boolean] = None
}

class ItemMod extends BaseItem {
  var vis: Option[Visibility]           = None
  var unsafe: Option[Boolean]           = None
  var ident: Ident                      = ""
  var content: Option[ListBuffer[Item]] = None
  var semi: Option[Boolean]             = None
}

class ItemStatic extends BaseItem {
  var vis: Option[Visibility]       = None
  var mut: Option[StaticMutability] = None
  var ident: Ident                  = ""
  var ty: Option[Type]              = None
  var expr: Option[Expr]            = None
}

class ItemStruct extends BaseItem {
  var vis: Option[Visibility]    = None
  var ident: Ident               = ""
  var generics: Option[Generics] = None
  var fields: Option[Fields]     = None
}

class ItemTrait extends BaseItem {
  var vis: Option[Visibility]                 = None
  var unsafe: Option[Boolean]                 = None
  var auto_token: Option[Boolean]             = None
  var restriction: Option[ImplRestriction]    = None
  var ident: Ident                            = ""
  var generics: Option[Generics]              = None
  var colon_token: Option[Boolean]            = None
  var supertraits: ListBuffer[TypeParamBound] = ListBuffer.empty
  var items: ListBuffer[TraitItem]            = ListBuffer.empty
}

class ItemTraitAlias extends BaseItem {
  var vis: Option[Visibility]            = None
  var ident: Ident                       = ""
  var generics: Option[Generics]         = None
  var bounds: ListBuffer[TypeParamBound] = ListBuffer.empty
}

class ItemType extends BaseItem {
  var vis: Option[Visibility]    = None
  var ident: Ident               = ""
  var generics: Option[Generics] = None
  var ty: Option[Type]           = None
}

class ItemUnion extends BaseItem {
  var vis: Option[Visibility]     = None
  var ident: Ident                = ""
  var generics: Option[Generics]  = None
  var fields: Option[FieldsNamed] = None
}

class ItemUse extends BaseItem {
  var vis: Option[Visibility]        = None
  var leading_colon: Option[Boolean] = None
  var tree: Option[UseTree]          = None
}
