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

trait AstForWherePredicate(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForWherePredicate(filename: String, parentFullname: String, wherePredicateInstance: WherePredicate): Ast = {
    if (wherePredicateInstance.lifetimeWherePredicate.isDefined) {
      astForLifetimeWherePredicate(filename, parentFullname, wherePredicateInstance.lifetimeWherePredicate.get)
    } else if (wherePredicateInstance.typeWherePredicate.isDefined) {
      astForTypeWherePredicate(filename, parentFullname, wherePredicateInstance.typeWherePredicate.get)
    } else {
      throw new RuntimeException(s"Unknown WherePredicate type: $wherePredicateInstance")
    }
  }

  def astForLifetimeWherePredicate(
    filename: String,
    parentFullname: String,
    lifetimeWherePredicateInstance: PredicateLifetime
  ): Ast = {
    val lifetimePredicateAst = astForLifetimeAsParam(filename, parentFullname, lifetimeWherePredicateInstance.lifetime)
    val boundsAst = lifetimeWherePredicateInstance.bounds.map(astForLifetimeAsParam(filename, parentFullname, _))

    var code =
      s"${codeForLifetime(filename, parentFullname, lifetimeWherePredicateInstance.lifetime)}"
    code = lifetimeWherePredicateInstance.bounds.nonEmpty match {
      case true =>
        s"$code: ${lifetimeWherePredicateInstance.bounds.map(codeForLifetime(filename, parentFullname, _)).mkString(" + ")}"
      case false => code
    }

    Ast(unknownNode(lifetimeWherePredicateInstance, code))
      .withChild(lifetimePredicateAst)
      .withChildren(boundsAst)
  }

  def astForTypeWherePredicate(
    filename: String,
    parentFullname: String,
    typeWherePredicateInstance: PredicateType
  ): Ast = {
    val boundedTypeAst = typeWherePredicateInstance.bounded_ty match {
      case Some(bounded_ty) => astForType(filename, parentFullname, bounded_ty)
      case None             => Ast()
    }
    val lifetimesBouldsAst = typeWherePredicateInstance.lifetimes match {
      case Some(lifetimes) => lifetimes.map(astForGenericParam(filename, parentFullname, _)).toList
      case None            => List()
    }
    val boundsAst = typeWherePredicateInstance.bounds.map(astForTypeParamBound(filename, parentFullname, _)).toList

    val boundedTypeCode = typeWherePredicateInstance.bounded_ty match {
      case Some(bounded_ty) => typeFullnameForType(filename, parentFullname, bounded_ty)
      case None             => Defines.Unknown
    }
    val boundsCode =
      typeWherePredicateInstance.bounds.map(codeForTypeParamBound(filename, parentFullname, _)).mkString(" + ")
    val code = s"$boundedTypeCode: $boundsCode"

    val node = NewTypeParameter()
      .name(boundedTypeCode)
      .code(code)

    Ast(unknownNode(typeWherePredicateInstance, ""))
      .withChild(Ast(node))
      .withChild(boundedTypeAst)
      .withChildren(boundsAst ++ lifetimesBouldsAst)
  }

  def astForLifetimeAsParam(filename: String, parentFullname: String, lifetimeInstance: Lifetime): Ast = {
    val code = codeForLifetime(filename, parentFullname, lifetimeInstance)
    val node = NewTypeParameter().name(lifetimeInstance).code(code)
    Ast(node)
  }

  def codeForLifetime(filename: String, parentFullname: String, lifetimeInstance: Lifetime): String = {
    s"'${lifetimeInstance}"
  }
}
