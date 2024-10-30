package io.joern.rustsrc2cpg.ast

import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSubTypes
import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.fasterxml.jackson.annotation.JsonTypeName
import com.fasterxml.jackson.annotation.JsonUnwrapped
import com.fasterxml.jackson.annotation.JsonValue
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.JsonDeserializer
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.BaseJsonNode
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.node.TextNode
import io.joern.rustsrc2cpg.ast.*

import scala.collection.mutable.ListBuffer

// Custom Type Deserializer
class TypeDeserializer extends JsonDeserializer[Type] {
  override def deserialize(p: JsonParser, ctx: DeserializationContext): Type = {

    val node = p.getCodec.readTree[BaseJsonNode](p)

    node match {
      case textNode: TextNode =>
        TypeNoValue.fromString(textNode.asText())
      case objectNode: ObjectNode =>
        val mapper = p.getCodec.asInstanceOf[ObjectMapper]
        mapper.treeToValue(objectNode, classOf[TypeHasValue])
      case unknown =>
        throw new IllegalArgumentException(s"Unexpected JSON node type ${unknown}")
    }
  }
}

sealed trait Type

enum TypeNoValue(val value: String) extends RustAst with Type {
  case TypeInfer extends TypeNoValue("_")
  case TypeNever extends TypeNoValue("!")

  @JsonValue
  override def toString: String = value

}

object TypeNoValue {
  @JsonCreator
  def fromString(value: String): TypeNoValue = value match {
    case "_" => TypeNoValue.TypeInfer
    case "!" => TypeNoValue.TypeNever
    case _   => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}

class TypeHasValue extends Type {
  @JsonProperty("array")
  var arrayType: Option[TypeArray] = None
  @JsonProperty("bare_fn")
  var bareFnType: Option[TypeBareFn] = None
  @JsonProperty("group")
  var groupType: Option[TypeGroup] = None
  @JsonProperty("impl_trait")
  var implTraitType: Option[TypeImplTrait] = None
  @JsonProperty("macro")
  var macroType: Option[TypeMacro] = None
  @JsonProperty("paren")
  var parenType: Option[TypeParen] = None
  @JsonProperty("path")
  var pathType: Option[TypePath] = None
  @JsonProperty("ptr")
  var ptrType: Option[TypePtr] = None
  @JsonProperty("reference")
  var referenceType: Option[TypeReference] = None
  @JsonProperty("slice")
  var sliceType: Option[TypeSlice] = None
  @JsonProperty("trait_object")
  var traitObjectType: Option[TypeTraitObject] = None
  @JsonProperty("tuple")
  var tupleType: Option[TypeTuple] = None
  @JsonProperty("verbatim")
  var verbatimType: Option[TokenStream] = None
}

class TypeArray extends RustAst {
  var elem: Option[Type] = None
  var len: Option[Expr]  = None
}

class TypeBareFn extends RustAst {
  var lifetimes: Option[BoundLifetimes] = None
  var unsafe: Option[Boolean]           = None
  var abi: Option[Abi]                  = None
  var inputs: ListBuffer[BareFnArg]     = ListBuffer.empty
  var variadic: Option[BareVariadic]    = None
  var output: ReturnType                = None
}

class TypeGroup extends RustAst {
  var elem: Option[Type] = None
}

class TypeImplTrait extends RustAst {
  var bounds: ListBuffer[TypeParamBound] = ListBuffer.empty
}

class TypeMacro extends RustAst with MacroEmbeddable {
  // @JsonProperty("macro")
  // var mac: Option[Macro] = None
}

class TypeParen extends RustAst {
  var elem: Option[Type] = None
}

class TypePath extends RustAst with PathEmbeddable {
  var qself: Option[QSelf] = None
  // var segments: ListBuffer[PathSegment] = ListBuffer.empty
}

class TypePtr extends RustAst {
  var const: Option[Boolean] = None
  var mut: Option[Boolean]   = None
  var elem: Option[Type]     = None
}

class TypeReference extends RustAst {
  var lifetime: Option[Lifetime] = None
  var mut: Option[Boolean]       = None
  var elem: Option[Type]         = None
}

class TypeSlice extends RustAst {
  var elem: Option[Type] = None
}

class TypeTraitObject extends RustAst {
  var dyn: Option[Boolean]               = None
  var bounds: ListBuffer[TypeParamBound] = ListBuffer.empty
}

class TypeTuple extends RustAst {
  var elems: ListBuffer[Type] = ListBuffer.empty
}
