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

abstract class RustAst {
  protected var _children: ListBuffer[RustAst] = ListBuffer.empty
  protected var _parent: Option[RustAst]       = None

  def setParent(parent: RustAst): Unit = {
    this._parent = Option(parent)
  }
  def parent: Option[RustAst] = this._parent

  def addChild(node: RustAst): Unit = {
    this._children.addOne(node)
  }
  def children: ListBuffer[RustAst] = this._children
}

class UnknownAst extends RustAst

class ExprElse extends RustAst {}

class FileAst extends RustAst {
  var shebang: Option[String]              = None
  var attrs: Option[ListBuffer[Attribute]] = None
  var items: ListBuffer[Item]              = ListBuffer.empty
}
