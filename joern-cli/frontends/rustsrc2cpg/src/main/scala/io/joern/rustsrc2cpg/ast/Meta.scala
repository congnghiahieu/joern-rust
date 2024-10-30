package io.joern.rustsrc2cpg.ast

import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonSetter
import com.fasterxml.jackson.annotation.JsonSubTypes
import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.fasterxml.jackson.annotation.JsonTypeName
import com.fasterxml.jackson.annotation.JsonUnwrapped
import com.fasterxml.jackson.annotation.JsonValue
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.JsonDeserializer
import com.fasterxml.jackson.databind.annotation.JsonDeserialize
import com.fasterxml.jackson.databind.node.ObjectNode
import io.joern.rustsrc2cpg.ast.*

import scala.collection.mutable.ListBuffer

class Meta {
  @JsonProperty("path")
  var path: Option[Path] = None
  @JsonProperty("list")
  var list: Option[MetaList] = None
  @JsonProperty("name_value")
  var nameValue: Option[MetaNameValue] = None
}

class PathSegmentDeserializer extends JsonDeserializer[ListBuffer[PathSegment]] {
  override def deserialize(p: JsonParser, ctx: DeserializationContext): ListBuffer[PathSegment] = {
    val node       = p.getCodec.readTree[ObjectNode](p)
    val listBuffer = ListBuffer[PathSegment]()

    node.elements().forEachRemaining { element =>
      val pathSegment = p.getCodec.treeToValue(element, classOf[PathSegment])
      listBuffer += pathSegment
    }

    listBuffer
  }
}

trait PathEmbeddable {
  private var _leading_colon: Option[Boolean]    = None
  private var _segments: ListBuffer[PathSegment] = ListBuffer.empty

  def leading_colon: Option[Boolean] = _leading_colon
  @JsonSetter("leading_colon")
  def setLeadingColon(value: Option[Boolean]): Unit = {
    _leading_colon = value
  }

  def segments: ListBuffer[PathSegment] = _segments
  @JsonSetter("segments")
  def setSegments(value: ListBuffer[PathSegment]): Unit = {
    _segments = value
  }
}

class Path extends RustAst with PathEmbeddable {
  @JsonCreator
  def this(
    @JsonProperty("segments") segments: ListBuffer[PathSegment] = ListBuffer.empty,
    @JsonProperty("leading_colon") leadingColon: Option[Boolean] = Some(false)
  ) = {
    this()
    setLeadingColon(leadingColon)
    setSegments(segments)
  }
}

class PathSegment extends RustAst {
  var ident: Ident                     = ""
  var arguments: Option[PathArguments] = None
}

class MetaList extends RustAst {
  var path: Option[Path]                = None
  var delimiter: Option[MacroDelimiter] = None
  var tokens: Option[TokenStream]       = None
}

class MetaNameValue extends RustAst {
  var path: Option[Path]  = None
  var value: Option[Expr] = None
}
