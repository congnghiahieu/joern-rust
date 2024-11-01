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

import scala.collection.mutable.ListBuffer

trait AstForFields(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForFields(filename: String, parentFullname: String, fieldsInstance: Fields): Ast = {
    fieldsInstance match {
      case fieldsNotUnit: FieldsNotUnit =>
        if (fieldsNotUnit.named.isDefined) {
          val fieldsNotUnitAst = unknownNode(UnknownAst(), "").parserTypeName("FieldsNamed")
          val fieldsAst        = fieldsNotUnit.named.get.map(astForField(filename, parentFullname, _))
          Ast(fieldsNotUnitAst).withChildren(fieldsAst)
        } else {
          val fieldsNotUnitAst = unknownNode(UnknownAst(), "").parserTypeName("FieldsUnnamed")
          val fieldsAst        = fieldsNotUnit.unnamed.get.map(astForField(filename, parentFullname, _))
          Ast(fieldsNotUnitAst).withChildren(fieldsAst)
        }
      case fieldsUnit: FieldsUnit =>
        val fieldsUnitAst = literalNode(fieldsUnit, Primitives.UNIT, "UNIT")
        Ast(fieldsUnitAst)
    }
  }

  def astForFieldPat(filename: String, parentFullname: String, fieldPatInstance: FieldPat): Ast = {
    val annotationsAst = fieldPatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val patAst = fieldPatInstance.pat match {
      case Some(pat) => astForPat(filename, parentFullname, pat)
      case None      => Ast()
    }

    val fieldIdent    = Member(fieldPatInstance.named, fieldPatInstance.unnamed)
    val fieldIdentAst = astForMember(filename, parentFullname, fieldIdent)

    val fieldName = codeForMember(filename, parentFullname, fieldIdent)
    val code      = codeForFieldPat(filename, parentFullname, fieldPatInstance)

    val node = memberNode(fieldPatInstance, fieldName, code, "")
    Ast(node)
      .withChild(fieldIdentAst)
      .withChild(patAst)
      .withChildren(annotationsAst)
  }

  def astForFieldValue(filename: String, parentFullname: String, fieldValueInstance: FieldValue): Ast = {
    val annotationsAst = fieldValueInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val exprAst = fieldValueInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }

    val fieldIdent    = Member(fieldValueInstance.named, fieldValueInstance.unnamed)
    val fieldIdentAst = astForMember(filename, parentFullname, fieldIdent)

    val fieldName = codeForMember(filename, parentFullname, fieldIdent)
    val code      = codeForFieldValue(filename, parentFullname, fieldValueInstance)

    val node = memberNode(fieldValueInstance, fieldName, code, "")
    Ast(node)
      .withChild(fieldIdentAst)
      .withChild(exprAst)
      .withChildren(annotationsAst)
  }

  def astForField(filename: String, parentFullname: String, fieldInstance: Field): Ast = {
    val annotationsAst = fieldInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val modifierNode = modifierForVisibility(filename, parentFullname, fieldInstance.vis)

    val typeAst = fieldInstance.ty match {
      case Some(ty) => astForType(filename, parentFullname, ty)
      case None     => Ast()
    }

    val typeFullname = fieldInstance.ty match {
      case Some(ty) => typeFullnameForType(filename, parentFullname, ty)
      case None     => Defines.Unknown
    }
    val code = codeForField(filename, parentFullname, fieldInstance)
    val node = memberNode(fieldInstance, code, code, typeFullname)
      .astParentFullName(parentFullname)

    fieldInstance.ident match {
      case Some(ident) => {
        val identNode =
          identifierNode(fieldInstance, ident, ident, typeFullname)
        Ast(node)
          .withChild(Ast(identNode))
          .withChild(typeAst)
          .withChild(Ast(modifierNode))
          .withChildren(annotationsAst)
      }
      case None =>
        Ast(node)
          .withChild(typeAst)
          .withChild(Ast(modifierNode))
          .withChildren(annotationsAst)
    }
  }
}

trait CodeForFields(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def codeForFields(filename: String, parentFullname: String, fieldsInstance: Fields): String = {
    fieldsInstance match {
      case fieldsNotUnit: FieldsNotUnit =>
        if (fieldsNotUnit.named.isDefined) {
          s"{${fieldsNotUnit.named.get.map(codeForField(filename, parentFullname, _)).mkString(", ")}}"
        } else {
          s"(${fieldsNotUnit.unnamed.get.map(codeForField(filename, parentFullname, _)).mkString(", ")})"
        }
      case fieldsUnit: FieldsUnit =>
        "UNIT"
    }
  }

  def codeForFieldPat(filename: String, parentFullname: String, fieldPatInstance: FieldPat): String = {
    val fieldIdent    = Member(fieldPatInstance.named, fieldPatInstance.unnamed)
    val fieldIdentAst = astForMember(filename, parentFullname, fieldIdent)
    val fieldName     = codeForMember(filename, parentFullname, fieldIdent)
    val patCode = fieldPatInstance.pat match {
      case Some(pat) => codeForPat(filename, parentFullname, pat)
      case None      => Defines.Unknown
    }

    val code = fieldPatInstance.colon_token match {
      case Some(true) => s"$fieldName: $patCode"
      case _          => fieldName
    }
    code
  }

  def codeForFieldValue(filename: String, parentFullname: String, fieldValueInstance: FieldValue): String = {
    val exprCode = fieldValueInstance.expr match {
      case Some(expr) => codeForExpr(filename, parentFullname, expr)
      case None       => Defines.Unknown
    }

    val fieldIdent = Member(fieldValueInstance.named, fieldValueInstance.unnamed)
    val fieldName  = codeForMember(filename, parentFullname, fieldIdent)
    val code = fieldValueInstance.colon_token match {
      case Some(true) => s"$fieldName: $exprCode"
      case _          => fieldName
    }
    code
  }

  def codeForField(filename: String, parentFullname: String, fieldInstance: Field): String = {
    val typeFullname = fieldInstance.ty match {
      case Some(ty) => typeFullnameForType(filename, parentFullname, ty)
      case None     => Defines.Unknown
    }
    val name = fieldInstance.ident match {
      case Some(ident) => ident
      case None        => typeFullname
    }
    val code = fieldInstance.colon_token match {
      case Some(true) => s"${name}: ${typeFullname}"
      case _          => name
    }
    code
  }
}
