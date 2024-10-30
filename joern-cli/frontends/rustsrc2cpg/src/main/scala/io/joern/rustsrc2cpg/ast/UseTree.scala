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

// Custom UseTree Deserializer
class UseTreeDeserializer extends JsonDeserializer[UseTree] {
  override def deserialize(p: JsonParser, ctx: DeserializationContext): UseTree = {

    val node = p.getCodec.readTree[BaseJsonNode](p)

    node match {
      case textNode: TextNode =>
        UseTreeGlob.fromString(textNode.asText())
      case objectNode: ObjectNode =>
        val mapper = p.getCodec.asInstanceOf[ObjectMapper]
        mapper.treeToValue(objectNode, classOf[UseTreeNotGlob])
      case unknown =>
        throw new IllegalArgumentException(s"Unexpected JSON node type ${unknown}")
    }
  }
}

sealed trait UseTree

enum UseTreeGlob(val value: String) extends RustAst with UseTree {
  case Glob extends UseTreeGlob("*")

  @JsonValue
  override def toString: String = value
}

object UseTreeGlob {
  @JsonCreator
  def fromString(value: String): UseTreeGlob = value match {
    case "*" => UseTreeGlob.Glob
    case _   => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}

class UseTreeNotGlob extends UseTree {
  @JsonProperty("path")
  var pathUseTree: Option[UsePath] = None
  @JsonProperty("ident")
  var ident: Option[UseName] = None
  @JsonProperty("rename")
  var renameUseTree: Option[UseRename] = None
  @JsonProperty("group")
  var groupUseTree: Option[UseGroup] = None
}

class UsePath extends RustAst {
  var ident: Ident          = ""
  var tree: Option[UseTree] = None
}

type UseGroup = ListBuffer[UseTree]

type UseName = Ident

class UseRename extends RustAst {
  var ident: Ident  = ""
  var rename: Ident = ""
}
