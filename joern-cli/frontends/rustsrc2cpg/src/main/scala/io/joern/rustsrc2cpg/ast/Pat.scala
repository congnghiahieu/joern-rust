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

class Pat {
  @JsonProperty("const")
  var constPat: Option[ExprConst] = None
  @JsonProperty("ident")
  var identPat: Option[PatIdent] = None
  @JsonProperty("lit")
  var litPat: Option[ExprLit] = None
  @JsonProperty("macro")
  var macroPat: Option[ExprMacro] = None
  @JsonProperty("or")
  var orPat: Option[PatOr] = None
  @JsonProperty("paren")
  var parenPat: Option[PatParen] = None
  @JsonProperty("path")
  var pathPat: Option[ExprPath] = None
  @JsonProperty("range")
  var rangePat: Option[ExprRange] = None
  @JsonProperty("reference")
  var referencePat: Option[PatReference] = None
  @JsonProperty("rest")
  var restPat: Option[PatRest] = None
  @JsonProperty("slice")
  var slicePat: Option[PatSlice] = None
  @JsonProperty("struct")
  var structPat: Option[PatStruct] = None
  @JsonProperty("tuple")
  var tuplePat: Option[PatTuple] = None
  @JsonProperty("tuple_struct")
  var tupleStructPat: Option[PatTupleStruct] = None
  @JsonProperty("type")
  var typePat: Option[PatType] = None
  @JsonProperty("verbatim")
  var verbatimPat: Option[TokenStream] = None
  @JsonProperty("_")
  var wildPat: Option[PatWild] = None
}

abstract class BasePat extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
}

class PatIdent extends BasePat {
  var ref: Option[Boolean] = None
  var mut: Option[Boolean] = None
  var ident: Ident         = ""
  var subpat: Option[Pat]  = None
}

class PatOr extends BasePat {
  var leading_vert: Option[Boolean] = None
  var cases: ListBuffer[Pat]        = ListBuffer.empty
}

class PatParen extends BasePat {
  var pat: Option[Pat] = None
}

class PatReference extends BasePat {
  var mut: Option[Boolean] = None
  var pat: Option[Pat]     = None
}

class PatRest extends BasePat

class PatSlice extends BasePat {
  var elems: ListBuffer[Pat] = ListBuffer.empty
}

class PatStruct extends BasePat {
  var qself: Option[QSelf]         = None
  var path: Option[Path]           = None
  var fields: ListBuffer[FieldPat] = ListBuffer.empty
  var rest: Option[PatRest]        = None
}

class PatTuple extends BasePat {
  var elems: ListBuffer[Pat] = ListBuffer.empty
}

class PatTupleStruct extends BasePat {
  var qself: Option[QSelf]   = None
  var path: Option[Path]     = None
  var elems: ListBuffer[Pat] = ListBuffer.empty
}

class PatType extends BasePat {
  var pat: Option[Pat] = None
  var ty: Option[Type] = None
}

class PatWild extends BasePat
