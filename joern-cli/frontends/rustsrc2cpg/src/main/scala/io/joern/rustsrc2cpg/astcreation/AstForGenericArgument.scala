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
    val code = codeForLifetime(filename, parentFullname, lifetimeInstance)
    val node = NewTypeArgument().code(code)
    Ast(node)
  }

  def astForTypeGenericArgument(filename: String, parentFullname: String, typeInstance: Type): Ast = {
    val newTypeArgument = NewTypeArgument()
    val typeAst         = astForType(filename, parentFullname, typeInstance)
    val code            = typeFullnameForType(filename, parentFullname, typeInstance)

    Ast(unknownNode(UnknownAst(), code))
      .withChild(Ast(newTypeArgument))
      .withChild(typeAst)
  }

  def astForConstGenericArgument(filename: String, parentFullname: String, constInstance: Expr): Ast = {
    val newTypeArgument = NewTypeArgument()
    val epxrAst         = astForExpr(filename, parentFullname, constInstance)
    val code            = codeForExpr(filename, parentFullname, constInstance)

    Ast(unknownNode(UnknownAst(), code))
      .withChild(Ast(newTypeArgument))
      .withChild(epxrAst)
  }

  def astForAssocTypeGenericArgument(filename: String, parentFullname: String, assocTypeInstance: AssocType): Ast = {
    val name            = assocTypeInstance.ident
    val newTypeArgument = NewTypeArgument()
    val genericAst = assocTypeInstance.generics match {
      case Some(generics) => astForAngleBracketedGenericArguments(filename, parentFullname, generics)
      case None           => Ast()
    }
    val typeAst = assocTypeInstance.ty match {
      case Some(ty) => astForType(filename, parentFullname, ty)
      case None     => Ast()
    }

    val code = codeForAssocTypeGenericArgument(filename, parentFullname, assocTypeInstance)

    Ast(unknownNode(assocTypeInstance, code))
      .withChild(Ast(newTypeArgument))
      .withChild(genericAst)
      .withChild(typeAst)
  }

  def astForAssocConstGenericArgument(filename: String, parentFullname: String, assocConstInstance: AssocConst): Ast = {
    val name            = assocConstInstance.ident
    val newTypeArgument = NewTypeArgument()
    val genericAst = assocConstInstance.generics match {
      case Some(generics) => astForAngleBracketedGenericArguments(filename, parentFullname, generics)
      case None           => Ast()
    }
    val valueAst = assocConstInstance.value match {
      case Some(value) => astForExpr(filename, parentFullname, value)
      case None        => Ast()
    }

    val code = codeForAssocConstGenericArgument(filename, parentFullname, assocConstInstance)

    Ast(unknownNode(assocConstInstance, code))
      .withChild(Ast(newTypeArgument))
      .withChild(genericAst)
      .withChild(valueAst)
  }

  def astForConstraintGenericArgument(filename: String, parentFullname: String, constraintInstance: Constraint): Ast = {
    val name            = constraintInstance.ident
    val newTypeArgument = NewTypeArgument()
    val genericAst = constraintInstance.generics match {
      case Some(generics) => astForAngleBracketedGenericArguments(filename, parentFullname, generics)
      case None           => Ast()
    }
    val boundsAst = constraintInstance.bounds.map(astForTypeParamBound(filename, parentFullname, _)).toList

    val code = codeForConstraintGenericArgument(filename, parentFullname, constraintInstance)

    Ast(unknownNode(constraintInstance, code))
      .withChild(Ast(newTypeArgument))
      .withChild(genericAst)
      .withChildren(boundsAst)
  }
}

trait CodeForGenericArgument(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def codeForGenericArgument(
    filename: String,
    parentFullname: String,
    genericArgumentInstance: GenericArgument
  ): String = {
    if (genericArgumentInstance.lifetimeGenericArgument.isDefined) {
      codeForLifetime(filename, parentFullname, genericArgumentInstance.lifetimeGenericArgument.get)
    } else if (genericArgumentInstance.typeGenericArgument.isDefined) {
      typeFullnameForType(filename, parentFullname, genericArgumentInstance.typeGenericArgument.get)
    } else if (genericArgumentInstance.constGenericArgument.isDefined) {
      codeForExpr(filename, parentFullname, genericArgumentInstance.constGenericArgument.get)
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

  def codeForAssocTypeGenericArgument(
    filename: String,
    parentFullname: String,
    assocTypeInstance: AssocType
  ): String = {
    val name = assocTypeInstance.ident
    var code = assocTypeInstance.generics match {
      case Some(generics) => {
        val angleCode = codeForAngleBracketedGenericArguments(filename, parentFullname, generics)
        s"$name$angleCode"
      }
      case None => name
    }
    code = assocTypeInstance.ty match {
      case Some(ty) => s"$code = ${typeFullnameForType(filename, parentFullname, ty)}"
      case None     => code
    }
    code
  }

  def codeForAssocConstGenericArgument(
    filename: String,
    parentFullname: String,
    assocConstInstance: AssocConst
  ): String = {
    var code = s"const ${assocConstInstance.ident}"
    code = assocConstInstance.generics match {
      case Some(generics) => {
        val angleCode = codeForAngleBracketedGenericArguments(filename, parentFullname, generics)
        s"$code$angleCode"
      }
      case None => code
    }
    code = assocConstInstance.value match {
      case Some(value) => {
        val exprCode = codeForExpr(filename, parentFullname, value)
        s"$code = $exprCode"
      }
      case None => code
    }
    code
  }

  def codeForConstraintGenericArgument(
    filename: String,
    parentFullname: String,
    constraintInstance: Constraint
  ): String = {
    var code = constraintInstance.ident
    code = constraintInstance.generics match {
      case Some(generics) => {
        val angleCode = codeForAngleBracketedGenericArguments(filename, parentFullname, generics)
        s"$code$angleCode"
      }
      case None => code
    }
    code = constraintInstance.bounds.nonEmpty match {
      case true =>
        s"$code: ${constraintInstance.bounds.map(codeForTypeParamBound(filename, parentFullname, _)).mkString(" + ")}"
      case false => code
    }
    code
  }
}
