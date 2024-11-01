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

trait AstForPathArguments(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForPathArguments(filename: String, parentFullname: String, pathArgumentsInstance: PathArguments): Ast = {
    pathArgumentsInstance match {
      case pathArgumentsNone: PathArgumentsNone =>
        val code         = pathArgumentsNone.toString
        val typeFullname = pathArgumentsNone.toString
        val node         = literalNode(UnknownAst(), code, typeFullname)
        Ast(node)
      case pathArgumentsNotNone: PathArgumentsNotNone =>
        astForPathArgumentsNotNone(filename, parentFullname, pathArgumentsNotNone)
    }
  }

  def astForPathArgumentsNotNone(
    filename: String,
    parentFullname: String,
    pathArgumentsNotNoneInstance: PathArgumentsNotNone
  ): Ast = {
    if (pathArgumentsNotNoneInstance.angleBracketed.isDefined) {
      astForAngleBracketedGenericArguments(filename, parentFullname, pathArgumentsNotNoneInstance.angleBracketed.get)
    } else if (pathArgumentsNotNoneInstance.parenthesized.isDefined) {
      astForParenthesizedGenericArguments(filename, parentFullname, pathArgumentsNotNoneInstance.parenthesized.get)
    } else {
      throw new IllegalArgumentException("Unsupported path arguments not none instance")
    }
  }

  def astForAngleBracketedGenericArguments(
    filename: String,
    parentFullname: String,
    angleBracketedInstance: AngleBracketedGenericArguments
  ): Ast = {
    val code    = codeForAngleBracketedGenericArguments(filename, parentFullname, angleBracketedInstance)
    val node    = NewTypeArgument().code(code)
    val argsAst = angleBracketedInstance.args.map(astForGenericArgument(filename, parentFullname, _))

    Ast(unknownNode(angleBracketedInstance, code))
      .withChild(Ast(node))
      .withChildren(argsAst)
  }

  def astForParenthesizedGenericArguments(
    filename: String,
    parentFullname: String,
    parenthesizedInstance: ParenthesizedGenericArguments
  ): Ast = {
    val code      = codeForParenthesizedGenericArguments(filename, parentFullname, parenthesizedInstance)
    val node      = NewTypeArgument().code(code)
    val inputsAst = parenthesizedInstance.inputs.map(astForType(filename, parentFullname, _))
    val outputAst = parenthesizedInstance.output match {
      case Some(output) => astForType(filename, parentFullname, output)
      case None         => Ast()
    }

    Ast(unknownNode(parenthesizedInstance, code))
      .withChild(Ast(node))
      .withChildren(inputsAst)
      .withChild(outputAst)
  }
}

trait CodeForPathArguments(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def codeForPathArguments(filename: String, parentFullname: String, pathArgumentsInstance: PathArguments): String = {
    pathArgumentsInstance match {
      case pathArgumentsNone: PathArgumentsNone =>
        pathArgumentsNone.toString
      case pathArgumentsNotNone: PathArgumentsNotNone =>
        codeForPathArgumentsNotNone(filename, parentFullname, pathArgumentsNotNone)
    }
  }

  def codeForPathArgumentsNotNone(
    filename: String,
    parentFullname: String,
    pathArgumentsNotNoneInstance: PathArgumentsNotNone
  ): String = {
    if (pathArgumentsNotNoneInstance.angleBracketed.isDefined) {
      codeForAngleBracketedGenericArguments(filename, parentFullname, pathArgumentsNotNoneInstance.angleBracketed.get)
    } else if (pathArgumentsNotNoneInstance.parenthesized.isDefined) {
      codeForParenthesizedGenericArguments(filename, parentFullname, pathArgumentsNotNoneInstance.parenthesized.get)
    } else {
      throw new IllegalArgumentException("Unsupported path arguments not none instance")
    }
  }

  def codeForAngleBracketedGenericArguments(
    filename: String,
    parentFullname: String,
    angleBracketedInstance: AngleBracketedGenericArguments
  ): String = {
    val codeArgs = angleBracketedInstance.args.map(codeForGenericArgument(filename, parentFullname, _))
    if (angleBracketedInstance.colon2_token.getOrElse(false)) {
      s"::<${codeArgs.mkString(", ")}>"
    } else {
      s"<${codeArgs.mkString(", ")}>"
    }
  }

  def codeForParenthesizedGenericArguments(
    filename: String,
    parentFullname: String,
    parenthesizedInstance: ParenthesizedGenericArguments
  ): String = {
    val inputTypeFullnames = parenthesizedInstance.inputs.map(typeFullnameForType(filename, parentFullname, _))
    val inputCode          = s"(${inputTypeFullnames.mkString(", ")})"
    parenthesizedInstance.output match {
      case Some(output) =>
        val outputTypeFullname = typeFullnameForType(filename, parentFullname, output)
        s"$inputCode -> $outputTypeFullname"
      case None =>
        inputCode
    }
  }
}
