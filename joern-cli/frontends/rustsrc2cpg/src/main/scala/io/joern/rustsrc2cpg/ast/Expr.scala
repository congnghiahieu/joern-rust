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

class Expr {
  @JsonProperty("array")
  var arrayExpr: Option[ExprArray] = None
  @JsonProperty("assign")
  var assignExpr: Option[ExprAssign] = None
  @JsonProperty("async")
  var asyncExpr: Option[ExprAsync] = None
  @JsonProperty("await")
  var awaitExpr: Option[ExprAwait] = None
  @JsonProperty("binary")
  var binaryExpr: Option[ExprBinary] = None
  @JsonProperty("block")
  var blockExpr: Option[ExprBlock] = None
  @JsonProperty("break")
  var breakExpr: Option[ExprBreak] = None
  @JsonProperty("call")
  var callExpr: Option[ExprCall] = None
  @JsonProperty("cast")
  var castExpr: Option[ExprCast] = None
  @JsonProperty("closure")
  var closureExpr: Option[ExprClosure] = None
  @JsonProperty("const")
  var constExpr: Option[ExprConst] = None
  @JsonProperty("continue")
  var continueExpr: Option[ExprContinue] = None
  @JsonProperty("field")
  var fieldExpr: Option[ExprField] = None
  @JsonProperty("for_loop")
  var forLoopExpr: Option[ExprForLoop] = None
  @JsonProperty("group")
  var groupExpr: Option[ExprGroup] = None
  @JsonProperty("if")
  var ifExpr: Option[ExprIf] = None
  @JsonProperty("index")
  var indexExpr: Option[ExprIndex] = None
  @JsonProperty("infer")
  var inferExpr: Option[ExprInfer] = None
  @JsonProperty("let")
  var letExpr: Option[ExprLet] = None
  @JsonProperty("lit")
  var litExpr: Option[ExprLit] = None
  @JsonProperty("loop")
  var loopExpr: Option[ExprLoop] = None
  @JsonProperty("macro")
  var macroExpr: Option[ExprMacro] = None
  @JsonProperty("match")
  var matchExpr: Option[ExprMatch] = None
  @JsonProperty("method_call")
  var methodCallExpr: Option[ExprMethodCall] = None
  @JsonProperty("paren")
  var parenExpr: Option[ExprParen] = None
  @JsonProperty("path")
  var pathExpr: Option[ExprPath] = None
  @JsonProperty("range")
  var rangeExpr: Option[ExprRange] = None
  @JsonProperty("reference")
  var referenceExpr: Option[ExprReference] = None
  @JsonProperty("repeat")
  var repeatExpr: Option[ExprRepeat] = None
  @JsonProperty("return")
  var returnExpr: Option[ExprReturn] = None
  @JsonProperty("struct")
  var structExpr: Option[ExprStruct] = None
  @JsonProperty("try")
  var tryExpr: Option[ExprTry] = None
  @JsonProperty("try_block")
  var tryBlockExpr: Option[ExprTryBlock] = None
  @JsonProperty("tuple")
  var tupleExpr: Option[ExprTuple] = None
  @JsonProperty("unary")
  var unaryExpr: Option[ExprUnary] = None
  @JsonProperty("unsafe")
  var unsafeExpr: Option[ExprUnsafe] = None
  @JsonProperty("verbatim")
  var verbatimExpr: Option[TokenStream] = None
  @JsonProperty("while")
  var whileExpr: Option[ExprWhile] = None
  @JsonProperty("yield")
  var yieldExpr: Option[ExprYield] = None
}

abstract class BaseExpr extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
}

class ExprArray extends BaseExpr {
  var elems: ListBuffer[Expr] = ListBuffer.empty
}

class ExprAssign extends BaseExpr {
  var left: Option[Expr]  = None
  var right: Option[Expr] = None
}

class ExprAsync extends BaseExpr {
  var move: Option[Boolean] = None
  var stmts: Block          = ListBuffer.empty
}

class ExprAwait extends BaseExpr {
  var base: Option[Expr] = None
}

class ExprBinary extends BaseExpr {
  var left: Option[Expr]  = None
  var op: Option[BinOp]   = None
  var right: Option[Expr] = None
}

class ExprBlock extends BaseExpr {
  var label: Option[Label] = None
  var stmts: Block         = ListBuffer.empty
}

class ExprBreak extends BaseExpr {
  var label: Option[Label] = None
  var expr: Option[Expr]   = None
}

class ExprCall extends BaseExpr {
  var func: Option[Expr]     = None
  var args: ListBuffer[Expr] = ListBuffer.empty
}

class ExprCast extends BaseExpr {
  var expr: Option[Expr] = None
  var ty: Option[Type]   = None
}

class ExprClosure extends BaseExpr {
  var lifetimes: Option[BoundLifetimes] = None
  var const: Option[Boolean]            = None
  var static: Option[Boolean]           = None
  var async: Option[Boolean]            = None
  var move: Option[Boolean]             = None
  var inputs: ListBuffer[Pat]           = ListBuffer.empty
  var output: ReturnType                = None
  var body: Option[Expr]                = None
}

class ExprConst extends BaseExpr {
  var stmts: Block = ListBuffer.empty
}

class ExprContinue extends BaseExpr {
  var label: Option[Label] = None
}

class ExprField extends BaseExpr with MemberEmbeddable {
  var base: Option[Expr] = None
  // @JsonUnwrapped
  // var member: Option[Member] = None
}

class ExprForLoop extends BaseExpr {
  var label: Option[Label] = None
  var pat: Option[Pat]     = None
  var expr: Option[Expr]   = None
  var body: Block          = ListBuffer.empty
}

class ExprGroup extends BaseExpr {
  var expr: Option[Expr] = None
}

class ExprIf extends BaseExpr {
  var cond: Option[Expr]        = None
  var then_branch: Block        = ListBuffer.empty
  var else_branch: Option[Expr] = None
}

class ExprIndex extends BaseExpr {
  var expr: Option[Expr]  = None
  var index: Option[Expr] = None
}

class ExprInfer extends BaseExpr

class ExprLet extends BaseExpr {
  var pat: Option[Pat]   = None
  var expr: Option[Expr] = None
}

class ExprLit extends BaseExpr with LitEmbeddable {
  // @JsonUnwrapped
  // var lit: Option[Lit] = None
}

class ExprLoop extends BaseExpr {
  var label: Option[Label] = None
  var body: Block          = ListBuffer.empty
}

class ExprMacro extends BaseExpr with MacroEmbeddable {
  // @JsonProperty("macro")
  // var mac: Option[Macro]          = None
}

class ExprMatch extends BaseExpr {
  var expr: Option[Expr]    = None
  var arms: ListBuffer[Arm] = ListBuffer.empty
}

class ExprMethodCall extends BaseExpr {
  var receiver: Option[Expr]                            = None
  var method: Ident                                     = ""
  var turbofish: Option[AngleBracketedGenericArguments] = None
  var args: ListBuffer[Expr]                            = ListBuffer.empty
}

class ExprParen extends BaseExpr {
  var expr: Option[Expr] = None
}

class ExprPath extends BaseExpr with PathEmbeddable {
  var qself: Option[QSelf] = None
  // @JsonUnwrapped
  // var path: Option[Path] = None
}

class ExprRange extends BaseExpr {
  var start: Option[Expr]         = None
  var limits: Option[RangeLimits] = None
  var end: Option[Expr]           = None
}

class ExprReference extends BaseExpr {
  var mut: Option[Boolean] = None
  var expr: Option[Expr]   = None
}

class ExprRepeat extends BaseExpr {
  var expr: Option[Expr] = None
  var len: Option[Expr]  = None
}

class ExprReturn extends BaseExpr {
  var expr: Option[Expr] = None
}

class ExprStruct extends BaseExpr {
  var qself: Option[QSelf]           = None
  var path: Option[Path]             = None
  var fields: ListBuffer[FieldValue] = ListBuffer.empty
  var dot2_token: Option[Boolean]    = None
  var rest: Option[Expr]             = None
}

class ExprTry extends BaseExpr {
  var expr: Option[Expr] = None
}

class ExprTryBlock extends BaseExpr {
  var stmts: Block = ListBuffer.empty
}

class ExprTuple extends BaseExpr {
  var elems: ListBuffer[Expr] = ListBuffer.empty
}

class ExprUnary extends BaseExpr {
  var op: Option[UnOp]   = None
  var expr: Option[Expr] = None
}

class ExprUnsafe extends BaseExpr {
  var stmts: Block = ListBuffer.empty
}

class ExprWhile extends BaseExpr {
  var label: Option[Label] = None
  var cond: Option[Expr]   = None
  var body: Block          = ListBuffer.empty
}

class ExprYield extends BaseExpr {
  var expr: Option[Expr] = None
}
