package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.collection.mutable.ListBuffer

type CodeForReturnType = (String, String, String)

trait AstForMeta(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForMeta(filename: String, parentFullname: String, metaInstance: Meta): Ast = {
    if (metaInstance.path.isDefined) {
      astForPath(filename, parentFullname, metaInstance.path.get)
    } else if (metaInstance.list.isDefined) {
      astForMetaList(filename, parentFullname, metaInstance.list.get)
    } else if (metaInstance.nameValue.isDefined) {
      astForMetaNameValue(filename, parentFullname, metaInstance.nameValue.get)
    } else {
      throw new IllegalArgumentException("Unsupported meta type")
    }
  }
  def astForPath(filename: String, parentFullname: String, pathInstance: Path): Ast = {
    val (typeFullname, _, code) = codeForPath(filename, parentFullname, pathInstance)
    val pathNode                = typeRefNode(pathInstance, code, typeFullname)
    Ast(pathNode)
  }

  def astForMetaList(filename: String, parentFullname: String, metaListInstance: MetaList): Ast = {
    val (typeFullname, inputToken, code) = codeForMetaList(filename, parentFullname, metaListInstance)
    val parameter                        = NewAnnotationParameter().code(inputToken)
    Ast(parameter)
  }

  def astForMetaNameValue(filename: String, parentFullname: String, metaNameValueInstance: MetaNameValue): Ast = {
    val (typeFullname, exprValue, code) = codeForMetaNameValue(filename, parentFullname, metaNameValueInstance)
    val parameterAssignNode             = NewAnnotationParameterAssign().code(code)
    annotationAssignmentAst(typeFullname, code, Ast(parameterAssignNode))
  }
}

trait CodeForMeta(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def codeForPath(filename: String, parentFullname: String, pathInstance: Path): CodeForReturnType = {
    val typeFullname = typeFullnameForPath(filename, parentFullname, pathInstance)
    val code         = typeFullname
    (typeFullname, "", code)
  }

  def codeForMetaList(filename: String, parentFullname: String, metaListInstance: MetaList): CodeForReturnType = {
    val typeFullname = metaListInstance.path match {
      case Some(path) => typeFullnameForPath(filename, parentFullname, path)
      case None       => ""
    }
    val inputToken = metaListInstance.tokens match {
      case Some(tokens) => codeForTokenStream(filename, parentFullname, tokens)
      case None         => ""
    }
    val code = metaListInstance.delimiter match {
      case Some(delimiter) => {
        delimiter match {
          case MacroDelimiter.Paren   => s"${inputToken}(${typeFullname})"
          case MacroDelimiter.Brace   => s"${inputToken}{${typeFullname}}"
          case MacroDelimiter.Bracket => s"${inputToken}[${typeFullname}]"
        }
      }
      case None => s"${typeFullname}${inputToken}"
    }

    (typeFullname, inputToken, code)
  }

  def codeForMetaNameValue(
    filename: String,
    parentFullname: String,
    metaNameValueInstance: MetaNameValue
  ): CodeForReturnType = {
    val typeFullname = metaNameValueInstance.path match {
      case Some(path) => typeFullnameForPath(filename, parentFullname, path)
      case None       => ""
    }

    val exprValue = metaNameValueInstance.value match {
      case Some(expr) => codeForExpr(filename, parentFullname, expr)
      case None       => ""
    }

    val code = metaNameValueInstance.value match {
      case Some(expr) => s"${typeFullname} = ${exprValue}"
      case None       => ""
    }

    (typeFullname, exprValue, code)
  }

  def typeFullnameForPath(filename: String, parentFullname: String, pathInstance: Path): String = {
    val (identParts, indetFullString) = typeFullnameForListPathSegment(filename, parentFullname, pathInstance.segments)
    pathInstance.leading_colon match {
      case Some(_) => s"::${indetFullString}"
      case None    => indetFullString
    }
  }

  def typeFullnameForListPathSegment(
    filename: String,
    parentFullname: String,
    listPathSegments: ListBuffer[PathSegment]
  ): (ListBuffer[String], String) = {
    val segmentIdents = listPathSegments.map(typeFullnameForPathSegment(filename, parentFullname, _))
    (segmentIdents, segmentIdents.mkString("::"))
  }

  def typeFullnameForPathSegment(filename: String, parentFullname: String, pathSegmentInstance: PathSegment): String = {
    pathSegmentInstance.arguments match {
      case Some(arguments) => {
        val args = codeForPathArguments(filename, parentFullname, arguments)
        s"${pathSegmentInstance.ident}${args}"
      }
      case None => pathSegmentInstance.ident
    }
  }
}