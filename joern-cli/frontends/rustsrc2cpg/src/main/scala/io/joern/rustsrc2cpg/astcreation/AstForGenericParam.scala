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

trait AstForGenericParam(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForGenericParam(filename: String, parentFullname: String, genericParamInstance: GenericParam): Ast = {
    if (genericParamInstance.lifetimeGenericParam.isDefined) {
      astForLifetimeGenericParam(filename, parentFullname, genericParamInstance.lifetimeGenericParam.get)
    } else if (genericParamInstance.typeGenericParam.isDefined) {
      astForTypeGenericParam(filename, parentFullname, genericParamInstance.typeGenericParam.get)
    } else if (genericParamInstance.constGenericParam.isDefined) {
      astForConstGenericParam(filename, parentFullname, genericParamInstance.constGenericParam.get)
    } else {
      throw new IllegalArgumentException("Unsupported generic param type")
    }
  }
  def astForLifetimeGenericParam(
    filename: String,
    parentFullname: String,
    lifetimeParamInstance: LifetimeParam
  ): Ast = {
    val annotationsAst = lifetimeParamInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val lifetimePredicateAst = astForLifetimeAsParam(filename, parentFullname, lifetimeParamInstance.lifetime)
    val boundsAst            = lifetimeParamInstance.bounds.map(astForLifetimeAsParam(filename, parentFullname, _))

    var code =
      s"${codeForLifetime(filename, parentFullname, lifetimeParamInstance.lifetime)}"
    code = lifetimeParamInstance.bounds.nonEmpty match {
      case true =>
        s"$code: ${lifetimeParamInstance.bounds.map(codeForLifetime(filename, parentFullname, _)).mkString(" + ")}"
      case false => code
    }

    Ast(unknownNode(lifetimeParamInstance, code))
      .withChild(lifetimePredicateAst)
      .withChildren(boundsAst)
      .withChildren(annotationsAst)
  }

  def astForTypeGenericParam(filename: String, parentFullname: String, typeParamInstance: TypeParam): Ast = {
    val annotationsAst = typeParamInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val boundsAst = typeParamInstance.bounds.map(astForTypeParamBound(filename, parentFullname, _))
    val deafaulAst = typeParamInstance.default match {
      case Some(default) => astForType(filename, parentFullname, default)
      case None          => Ast()
    }

    var code = typeParamInstance.ident
    code = typeParamInstance.bounds.nonEmpty match {
      case true =>
        s"$code: ${typeParamInstance.bounds.map(codeForTypeParamBound(filename, parentFullname, _)).mkString(" + ")}"
      case false => code
    }
    code = typeParamInstance.default match {
      case Some(default) => s"$code = ${typeFullnameForType(filename, parentFullname, default)}"
      case None          => code
    }

    val typeParameterNode = NewTypeParameter()
      .name(typeParamInstance.ident)
      .code(code)

    Ast(unknownNode(typeParamInstance, code))
      .withChild(Ast(typeParameterNode))
      .withChildren(boundsAst)
      .withChild(deafaulAst)
      .withChildren(annotationsAst)
  }

  def astForConstGenericParam(filename: String, parentFullname: String, constParamInstance: ConstParam): Ast = {
    val annotationsAst = constParamInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val typeAst = constParamInstance.ty match {
      case Some(ty) => astForType(filename, parentFullname, ty)
      case None     => Ast()
    }
    val deafaulAst = constParamInstance.default match {
      case Some(default) => astForExpr(filename, parentFullname, default)
      case None          => Ast()
    }

    val typeFullname = constParamInstance.ty match {
      case Some(ty) => typeFullnameForType(filename, parentFullname, ty)
      case None     => Defines.Unknown
    }
    var code = s"const ${constParamInstance.ident}: $typeFullname"
    code = constParamInstance.default match {
      case Some(default) => s"$code = ${codeForExpr(filename, parentFullname, default)}"
      case None          => code
    }

    val typeParameterNode = NewTypeParameter()
      .name(constParamInstance.ident)
      .code(code)

    Ast(unknownNode(constParamInstance, code))
      .withChild(Ast(typeParameterNode))
      .withChild(typeAst)
      .withChild(deafaulAst)
      .withChildren(annotationsAst)
  }
}
