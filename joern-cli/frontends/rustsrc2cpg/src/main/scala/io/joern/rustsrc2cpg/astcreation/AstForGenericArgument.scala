package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
import io.joern.rustsrc2cpg.ast.Type
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

trait AstForGenericArgument(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForGenericArgument(filename: String, parentFullname: String, genericArgumentInstance: GenericArgument): Ast = {
    if (genericArgumentInstance.lifetimeGenericArgument.isDefined) {
      astForLifetimeGenericArgument(filename, parentFullname, genericArgumentInstance.lifetimeGenericArgument.get)
    } else if (genericArgumentInstance.typeGenericArgument.isDefined) {
      astForTypeGenericArgument(filename, parentFullname, genericArgumentInstance.typeGenericArgument.get)
    } else if (genericArgumentInstance.constGenericArgument.isDefined) {
      astForConstGenericArgument(filename, parentFullname, genericArgumentInstance.constGenericArgument.get)
    } else if (genericArgumentInstance.assocTypeGenericArgument.isDefined) {
      astForAssocTypeGenericArgument(filename, parentFullname, genericArgumentInstance.assocTypeGenericArgument.get)
    } else if (genericArgumentInstance.assocConstGenericArgument.isDefined) {
      astForAssocConstGenericArgument(filename, parentFullname, genericArgumentInstance.assocConstGenericArgument.get)
    } else if (genericArgumentInstance.constraintGenericArgument.isDefined) {
      astForConstraintGenericArgument(filename, parentFullname, genericArgumentInstance.constraintGenericArgument.get)
    } else {
      throw new IllegalArgumentException("Unsupported generic argument type")
    }
  }

  def astForLifetimeGenericArgument(filename: String, parentFullname: String, lifetimeInstance: Lifetime): Ast = {
    astForLifetimeAsArgument(filename, parentFullname, lifetimeInstance)
  }

  def astForTypeGenericArgument(filename: String, parentFullname: String, typeInstance: Type): Ast = {
    val newTypeArgument = NewTypeArgument()
    Ast(newTypeArgument)
  }

  def astForConstGenericArgument(filename: String, parentFullname: String, constInstance: Expr): Ast = {
    val newTypeArgument = NewTypeArgument()
    Ast(newTypeArgument)
  }

  def astForAssocTypeGenericArgument(filename: String, parentFullname: String, assocTypeInstance: AssocType): Ast = {
    val newTypeArgument = NewTypeArgument()
    Ast(newTypeArgument)
  }

  def astForAssocConstGenericArgument(filename: String, parentFullname: String, assocConstInstance: AssocConst): Ast = {
    val newTypeArgument = NewTypeArgument()
    Ast(newTypeArgument)
  }

  def astForConstraintGenericArgument(filename: String, parentFullname: String, constraintInstance: Constraint): Ast = {
    val newTypeArgument = NewTypeArgument()
    Ast(newTypeArgument)
  }
}

trait CodeForGenericArgument(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def codeForGenericArgument(
    filename: String,
    parentFullname: String,
    genericArgumentInstance: GenericArgument
  ): String = {
    if (genericArgumentInstance.lifetimeGenericArgument.isDefined) {
      codeForLifetimeGenericArgument(filename, parentFullname, genericArgumentInstance.lifetimeGenericArgument.get)
    } else if (genericArgumentInstance.typeGenericArgument.isDefined) {
      codeForTypeGenericArgument(filename, parentFullname, genericArgumentInstance.typeGenericArgument.get)
    } else if (genericArgumentInstance.constGenericArgument.isDefined) {
      codeForConstGenericArgument(filename, parentFullname, genericArgumentInstance.constGenericArgument.get)
    } else if (genericArgumentInstance.assocTypeGenericArgument.isDefined) {
      codeForAssocTypeGenericArgument(filename, parentFullname, genericArgumentInstance.assocTypeGenericArgument.get)
    } else if (genericArgumentInstance.assocConstGenericArgument.isDefined) {
      codeForAssocConstGenericArgument(filename, parentFullname, genericArgumentInstance.assocConstGenericArgument.get)
    } else if (genericArgumentInstance.constraintGenericArgument.isDefined) {
      codeForConstraintGenericArgument(filename, parentFullname, genericArgumentInstance.constraintGenericArgument.get)
    } else {
      throw new IllegalArgumentException("Unsupported generic argument type")
    }
  }

  def codeForLifetimeGenericArgument(filename: String, parentFullname: String, lifetimeInstance: Lifetime): String = {
    codeForLifetime(filename, parentFullname, lifetimeInstance)
  }

  def codeForTypeGenericArgument(filename: String, parentFullname: String, typeInstance: Type): String = {
    typeFullnameForType(filename, parentFullname, typeInstance)
  }

  def codeForConstGenericArgument(filename: String, parentFullname: String, constInstance: Expr): String = {
    codeForExpr(filename, parentFullname, constInstance)
  }

  def codeForAssocTypeGenericArgument(
    filename: String,
    parentFullname: String,
    assocTypeInstance: AssocType
  ): String = {
    ""
  }

  def codeForAssocConstGenericArgument(
    filename: String,
    parentFullname: String,
    assocConstInstance: AssocConst
  ): String = {
    ""
  }

  def codeForConstraintGenericArgument(
    filename: String,
    parentFullname: String,
    constraintInstance: Constraint
  ): String = {
    ""
  }
}
