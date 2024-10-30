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

// Custom PathArguments Deserializer
class PathArgumentsDeserializer extends JsonDeserializer[PathArguments] {
  override def deserialize(p: JsonParser, ctx: DeserializationContext): PathArguments = {

    val node = p.getCodec.readTree[BaseJsonNode](p)

    node match {
      case textNode: TextNode =>
        PathArgumentsNone.fromString(textNode.asText())
      case objectNode: ObjectNode =>
        val mapper = p.getCodec.asInstanceOf[ObjectMapper]
        mapper.treeToValue(objectNode, classOf[PathArgumentsNotNone])
      case unknown =>
        throw new IllegalArgumentException(s"Unexpected JSON node type ${unknown}")
    }
  }
}

sealed trait PathArguments

enum PathArgumentsNone(val value: String) extends RustAst with PathArguments {
  case None extends PathArgumentsNone("")

  @JsonValue
  override def toString: String = value
}

object PathArgumentsNone {
  @JsonCreator
  def fromString(value: String): PathArgumentsNone = value match {
    case "" => PathArgumentsNone.None
    case _  => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}

class PathArgumentsNotNone extends PathArguments {
  @JsonProperty("angle_bracketed")
  var angleBracketed: Option[AngleBracketedGenericArguments] = None
  @JsonProperty("parenthesized")
  var parenthesized: Option[ParenthesizedGenericArguments] = None
}

class AngleBracketedGenericArguments extends RustAst {
  var colon2_token: Option[Boolean]     = None
  var args: ListBuffer[GenericArgument] = ListBuffer.empty
}

class ParenthesizedGenericArguments extends RustAst {
  var inputs: ListBuffer[Type] = ListBuffer.empty
  var output: ReturnType       = None
}
