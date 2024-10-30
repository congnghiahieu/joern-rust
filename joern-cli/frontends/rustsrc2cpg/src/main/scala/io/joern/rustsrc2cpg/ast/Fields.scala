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

// Custom Fields Deserializer
class FieldsDeserializer extends JsonDeserializer[Fields] {
  override def deserialize(p: JsonParser, ctx: DeserializationContext): Fields = {

    val node = p.getCodec.readTree[BaseJsonNode](p)

    node match {
      case textNode: TextNode =>
        FieldsUnit.fromString(textNode.asText())
      case objectNode: ObjectNode =>
        val mapper = p.getCodec.asInstanceOf[ObjectMapper]
        mapper.treeToValue(objectNode, classOf[FieldsNotUnit])
      case unknown =>
        throw new IllegalArgumentException(s"Unexpected JSON node type ${unknown}")
    }
  }
}

sealed trait Fields

enum FieldsUnit(val value: String) extends RustAst with Fields {
  case Unit extends FieldsUnit("unit")

  @JsonValue
  override def toString: String = value
}

object FieldsUnit {
  @JsonCreator
  def fromString(value: String): FieldsUnit = value match {
    case "unit" => FieldsUnit.Unit
    case _      => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}

type FieldsNamed   = ListBuffer[Field]
type FieldsUnnamed = ListBuffer[Field]

class FieldsNotUnit extends Fields {
  @JsonProperty("named")
  var named: Option[FieldsNamed] = None
  @JsonProperty("unnamed")
  var unnamed: Option[FieldsUnnamed] = None
}

enum FieldMutability(val value: String) extends RustAst {
  case None extends FieldMutability("")

  @JsonValue
  override def toString: String = value
}

object FieldMutability {
  @JsonCreator
  def fromString(value: String): FieldMutability = value match {
    case "" => FieldMutability.None
    case _  => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}

class FieldPat extends RustAst with MemberEmbeddable {
  var attrs: Option[ListBuffer[Attribute]] = None
  // @JsonUnwrapped
  // var member: Option[Member]       = None
  var colon_token: Option[Boolean] = None
  var pat: Option[Pat]             = None
}

class FieldValue extends RustAst with MemberEmbeddable {
  var attrs: Option[ListBuffer[Attribute]] = None
  // @JsonUnwrapped
  // var member: Option[Member]       = None
  var colon_token: Option[Boolean] = None
  var expr: Option[Expr]           = None
}

class Field extends RustAst {
  var attrs: Option[ListBuffer[Attribute]] = None
  var vis: Option[Visibility]              = None
  var mut: Option[FieldMutability]         = None
  var ident: Option[Ident]                 = None
  var colon_token: Option[Boolean]         = None
  var ty: Option[Type]                     = None
}
