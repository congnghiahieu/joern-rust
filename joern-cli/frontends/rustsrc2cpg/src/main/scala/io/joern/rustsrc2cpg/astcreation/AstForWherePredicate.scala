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

    lifetimePredicateAst
    // .withChildren(boundsAst)
  }

  def astForTypeWherePredicate(
    filename: String,
    parentFullname: String,
    typeWherePredicateInstance: PredicateType
  ): Ast = {
    val node = NewTypeParameter()
    Ast(node)
  }

  def astForLifetimeAsParam(filename: String, parentFullname: String, lifetimeInstance: Lifetime): Ast = {
    val code = codeForLifetime(filename, parentFullname, lifetimeInstance)
    val node = NewTypeParameter().name(lifetimeInstance).code(code)
    Ast(node)
  }

  def astForLifetimeAsArgument(filename: String, parentFullname: String, lifetimeInstance: Lifetime): Ast = {
    val code = codeForLifetime(filename, parentFullname, lifetimeInstance)
    val node = NewTypeArgument().code(code)
    Ast(node)
  }

  def codeForLifetime(filename: String, parentFullname: String, lifetimeInstance: Lifetime): String = {
    s"'${lifetimeInstance}'"
  }
}
