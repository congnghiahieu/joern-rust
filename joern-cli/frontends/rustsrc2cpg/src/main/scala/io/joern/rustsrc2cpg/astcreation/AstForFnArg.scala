package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.utils.NodeBuilders.newThisParameterNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.collection.mutable.ListBuffer

trait AstForFnArg(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>

  def astForFnArg(filename: String, parentFullname: String, fnArg: FnArg): Ast = {
    if (fnArg.receiverFnArg.isDefined) {
      return astForReceiver(filename, parentFullname, fnArg.receiverFnArg.get)
    } else if (fnArg.typedFnArg.isDefined) {
      return astForFnArgPatType(filename, parentFullname, fnArg.typedFnArg.get)
    } else {
      throw new RuntimeException(s"Unknown fnArg type: $fnArg")
    }
  }

  def astForFnArgPatType(filename: String, parentFullname: String, patTypeInstance: PatType): Ast = {
    val node = parameterInNode(patTypeInstance, "", "", 0, false, EvaluationStrategies.BY_VALUE, "")
    Ast(node)
  }

  def astForReceiver(filename: String, parentFullname: String, receiverInstance: Receiver): Ast = {
    val node = parameterInNode(receiverInstance, "", "", 0, false, EvaluationStrategies.BY_VALUE, "")
    val annotationsAst = receiverInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    Ast(node).withChildren(annotationsAst)
  }

  def astForBareFnArg(filename: String, parentFullname: String, bareFnArgInstance: BareFnArg): Ast = {
    val annotationsAst = bareFnArgInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val node = parameterInNode(bareFnArgInstance, "", "", 0, false, EvaluationStrategies.BY_VALUE, "")
    Ast(node).withChildren(annotationsAst)
  }

  def astForBareVariadic(filename: String, parentFullname: String, bareVariadicInstance: BareVariadic): Ast = {
    val annotationsAst = bareVariadicInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val node = parameterInNode(bareVariadicInstance, "", "", 0, false, EvaluationStrategies.BY_VALUE, "")
    Ast(node).withChildren(annotationsAst)
  }
}
