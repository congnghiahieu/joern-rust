package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.Defines
import io.joern.x2cpg.Defines.Unknown
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.utils.NodeBuilders.newThisParameterNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.collection.mutable.ListBuffer

trait AstForFn(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>

  def astForReturnType(filename: String, parentFullname: String, returnTypeInstance: ReturnType): Ast = {
    if (!returnTypeInstance.isDefined) {
      return Ast(unknownNode(UnknownAst(), ""))
    }

    return astForType(filename, parentFullname, returnTypeInstance.get)
  }

  def astForVariadic(filename: String, parentFullname: String, variadicInstance: Variadic): Ast = {
    val annotationsAst = variadicInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val name = variadicInstance.pat match {
      case Some(pat) => codeForPat(filename, parentFullname, pat)
      case None      => Defines.Unknown
    }
    val code = variadicInstance.comma match {
      case Some(true) => s"$name: ...,"
      case _          => s"$name: ..."
    }

    val node = parameterInNode(variadicInstance, name, code, -1, true, EvaluationStrategies.BY_VALUE, "")

    Ast(node)
      .withChildren(annotationsAst)
  }

  def astForVariant(filename: String, parentFullname: String, variantInstance: Variant): Ast = {
    val name = variantInstance.ident
    var code = variantInstance.ident
    code = variantInstance.discriminant match {
      case Some(discriminant) => s"$code = ${codeForExpr(filename, parentFullname, discriminant)}"
      case None               => code
    }
    val typeFullname = s"${parentFullname}::${variantInstance.ident}"

    val annotationsAst = variantInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, typeFullname, _)).toList
      case None        => List()
    }
    val node = memberNode(variantInstance, name, code, typeFullname)
      .astParentFullName(parentFullname)
      .astParentType(classOf[ItemEnum].getSimpleName)

    variantInstance.discriminant match {
      case Some(discriminant) => {
        val disciminantAst = astForExpr(filename, typeFullname, discriminant)
        Ast(node)
          .withChild(disciminantAst)
          .withChildren(annotationsAst)
      }
      case None => {
        val fieldsAst = variantInstance.fields match {
          case Some(fields) => astForFields(filename, typeFullname, fields)
          case None         => Ast()
        }
        Ast(node)
          .withChild(fieldsAst)
          .withChildren(annotationsAst)
      }
    }

  }
}
