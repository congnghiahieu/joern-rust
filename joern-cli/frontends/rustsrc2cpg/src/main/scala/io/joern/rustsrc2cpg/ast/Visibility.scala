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

// Custom Visibility Deserializer
class VisibilityDeserializer extends JsonDeserializer[Visibility] {
  override def deserialize(p: JsonParser, ctx: DeserializationContext): Visibility = {

    val node = p.getCodec.readTree[BaseJsonNode](p)

    node match {
      case textNode: TextNode =>
        VisibilityString.fromString(textNode.asText())
      case objectNode: ObjectNode =>
        val mapper = p.getCodec.asInstanceOf[ObjectMapper]
        mapper.treeToValue(objectNode, classOf[VisibilityOther])
      case unknown =>
        throw new IllegalArgumentException(s"Unexpected JSON node type ${unknown}")
    }
  }
}

sealed trait Visibility

enum VisibilityString(val value: String) extends RustAst with Visibility {
  case VisibilityPublic    extends VisibilityString("pub")
  case VisibilityInherited extends VisibilityString("inherited")

  @JsonValue
  override def toString: String = value
}

object VisibilityString {
  @JsonCreator
  def fromString(value: String): VisibilityString = value match {
    case "pub"       => VisibilityString.VisibilityPublic
    case "inherited" => VisibilityString.VisibilityInherited
    case _           => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}

class VisibilityOther extends Visibility {
  @JsonProperty("restricted")
  var restricted: Option[VisibilityRestricted] = None
}

class VisibilityRestricted extends RustAst {
  var in_token: Option[Boolean] = None
  var path: Option[Path]        = None
}
