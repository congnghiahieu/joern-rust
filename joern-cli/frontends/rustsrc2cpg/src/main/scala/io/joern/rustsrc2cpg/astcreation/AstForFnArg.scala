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
    val annotationsAst = patTypeInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val patAst = patTypeInstance.pat match {
      case Some(pat) => astForPat(filename, parentFullname, pat)
      case None      => Ast()
    }
    val typeAst = patTypeInstance.ty match {
      case Some(ty) => astForType(filename, parentFullname, ty)
      case None     => Ast()
    }

    val code         = codeForPatType(filename, parentFullname, patTypeInstance)
    val name         = code.split(":").map(_.trim).head
    val typeFullname = code.split(":").map(_.trim).last
    val evaluationStrategy = typeFullname.contains("&") match {
      case true  => EvaluationStrategies.BY_REFERENCE
      case false => EvaluationStrategies.BY_VALUE
    }
    val node = parameterInNode(patTypeInstance, name, code, -1, false, evaluationStrategy, typeFullname)

    Ast(node)
      .withChild(patAst)
      .withChild(typeAst)
      .withChildren(annotationsAst)
  }

  def astForReceiver(filename: String, parentFullname: String, receiverInstance: Receiver): Ast = {
    val annotationsAst = receiverInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val evaluationStrategy = receiverInstance.ref match {
      case Some(true) => EvaluationStrategies.BY_REFERENCE
      case _          => EvaluationStrategies.BY_VALUE
    }
    val name = "self"
    val typeFullname = receiverInstance.ty match {
      case Some(ty) => typeFullnameForType(filename, parentFullname, ty)
      case None     => Defines.Unknown
    }

    var codePrefix = receiverInstance.ref match {
      case Some(true) => s"&"
      case _          => s""
    }
    codePrefix = receiverInstance.lifetime match {
      case Some(lifetime) => s"${codePrefix}${codeForLifetime(filename, parentFullname, lifetime)}"
      case _              => codePrefix
    }
    codePrefix = receiverInstance.mut match {
      case Some(true) => s"${codePrefix} mut"
      case _          => codePrefix
    }
    val code = s"${codePrefix} self"

    val node = parameterInNode(receiverInstance, name, code, -1, false, evaluationStrategy, typeFullname)
    Ast(node)
      .withChildren(annotationsAst)
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
