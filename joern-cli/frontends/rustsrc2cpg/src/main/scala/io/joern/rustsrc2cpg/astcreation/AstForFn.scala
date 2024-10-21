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
      return Ast(unknownNode(EmptyAst(), ""))
    }

    return astForType(filename, parentFullname, returnTypeInstance.get)
  }

  def astForVariadic(filename: String, parentFullname: String, variadicInstance: Variadic): Ast = {
    val annotationsAst = variadicInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val node = parameterInNode(variadicInstance, "", "", 0, false, EvaluationStrategies.BY_VALUE, "")
    Ast(node).withChildren(annotationsAst)
  }

  def astForVariant(filename: String, parentFullname: String, variantInstance: Variant): Ast = {
    val annotationsAst = variantInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val node = memberNode(variantInstance, "", "", "")
    Ast(node).withChildren(annotationsAst)
  }
}
