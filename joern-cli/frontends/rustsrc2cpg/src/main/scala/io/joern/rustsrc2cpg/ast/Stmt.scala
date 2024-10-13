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

class Stmt {
  @JsonProperty("let")
  var letStmt: Option[Local] = None
  @JsonProperty("item")
  var itemStmt: Option[Item] = None
  @JsonProperty("expr")
  var exprStmt: Option[(Expr, Boolean)] = None
  @JsonProperty("macro")
  var macroStmt: Option[StmtMacro] = None
}

class StmtMacro extends RustAst with MacroEmbeddable {
  var attrs: Option[ListBuffer[Attribute]] = None
  // @JsonProperty("macro")
  // var mac: Option[Macro]          = None
  var semi_token: Option[Boolean] = None
}

class Local extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
  var pat: Option[Pat]                     = None
  var init: Option[LocalInit]              = None
}

class LocalInit extends RustAst {
  var expr: Option[Expr]    = None
  var diverge: Option[Expr] = None
}

type Block = ListBuffer[Stmt]
