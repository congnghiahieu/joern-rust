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

trait AstForFields(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForFields(filename: String, parentFullname: String, fieldsInstance: Fields): Ast = {
    fieldsInstance match {
      case fieldsNotUnit: FieldsNotUnit =>
        if (fieldsNotUnit.named.isDefined) {
          val fieldsNotUnitAst = unknownNode(EmptyAst(), "")
          val fieldsAst        = fieldsNotUnit.named.get.map(astForField(filename, parentFullname, _))
          Ast(fieldsNotUnitAst).withChildren(fieldsAst)
        } else {
          val fieldsNotUnitAst = unknownNode(EmptyAst(), "")
          val fieldsAst        = fieldsNotUnit.unnamed.get.map(astForField(filename, parentFullname, _))
          Ast(fieldsNotUnitAst).withChildren(fieldsAst)
        }
      case fieldsUnit: FieldsUnit =>
        val fieldsNotUnitAst = unknownNode(fieldsUnit, fieldsUnit.toString)
        Ast(fieldsNotUnitAst)
    }
  }

  def astForFieldPat(filename: String, parentFullname: String, fieldPatInstance: FieldPat): Ast = {
    val annotationsAst = fieldPatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val node = memberNode(fieldPatInstance, "", "", "")
    Ast(node).withChildren(annotationsAst)
  }

  def astForFieldValue(filename: String, parentFullname: String, fieldValueInstance: FieldValue): Ast = {
    val annotationsAst = fieldValueInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val node = memberNode(fieldValueInstance, "", "", "")
    Ast(node).withChildren(annotationsAst)
  }

  def astForField(filename: String, parentFullname: String, fieldInstance: Field): Ast = {
    val annotationsAst = fieldInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val node = memberNode(fieldInstance, "", "", "")
    Ast(node).withChildren(annotationsAst)
  }
}
