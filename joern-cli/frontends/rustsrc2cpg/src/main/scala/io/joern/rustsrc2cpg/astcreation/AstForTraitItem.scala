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

trait AstForTraitItem(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForTraitItem(filename: String, parentFullname: String, traitItemInstance: TraitItem): Ast = {
    if (traitItemInstance.constTraitItem.isDefined) {
      return astForTraitItemConst(filename, parentFullname, traitItemInstance.constTraitItem.get)
    } else if (traitItemInstance.fnTraitItem.isDefined) {
      return astForTraitItemFn(filename, parentFullname, traitItemInstance.fnTraitItem.get)
    } else if (traitItemInstance.typeTraitItem.isDefined) {
      return astForTraitItemType(filename, parentFullname, traitItemInstance.typeTraitItem.get)
    } else if (traitItemInstance.macroTraitItem.isDefined) {
      return astForTraitItemMacro(filename, parentFullname, traitItemInstance.macroTraitItem.get)
    } else if (traitItemInstance.verbatimTraitItem.isDefined) {
      return astForTokenStream(filename, parentFullname, traitItemInstance.verbatimTraitItem.get)
    } else {
      throw new RuntimeException("Unknown trait item type")
    }
  }

  def astForTraitItemConst(filename: String, parentFullname: String, traitItemConst: TraitItemConst): Ast = {
    val annotationsAst = traitItemConst.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val code = ""
    val typeFullName = traitItemConst.ty match {
      case Some(ty) => typeFullnameForType(filename, parentFullname, ty)
      case None     => ""
    }
    val newLocal = localNode(traitItemConst, traitItemConst.ident, code, typeFullName)

    Ast(memberNode(EmptyAst(), "", "", ""))
      .withChild(
        Ast(newLocal)
        // .withChildren(annotationsAst)
      )
  }

  def astForTraitItemFn(filename: String, parentFullname: String, traitItemFn: TraitItemFn): Ast = {
    val annotationsAst = traitItemFn.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val bodyAst       = blockAst(blockNode(traitItemFn, "", filename))
    val newMethodNode = methodNode(traitItemFn, traitItemFn.ident, traitItemFn.ident, "", filename)
    val parameterIns  = traitItemFn.inputs.map(input => astForFnArg(filename, parentFullname, input)).toList
    val methodReturnTypeFullname = traitItemFn.output match {
      case Some(output) => typeFullnameForType(filename, parentFullname, output)
      case None         => ""
    }

    val methodRetNode = methodReturnNode(traitItemFn, methodReturnTypeFullname)
    val methodAst =
      methodAstWithAnnotations(newMethodNode, parameterIns, bodyAst, methodRetNode, Nil, annotationsAst)

    Ast(memberNode(EmptyAst(), "", "", ""))
      .withChild(methodAst)
  }

  def astForTraitItemType(filename: String, parentFullname: String, traitItemType: TraitItemType): Ast = {
    val annotationsAst = traitItemType.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val code        = ""
    val newTypeDecl = typeDeclNode(traitItemType, traitItemType.ident, traitItemType.ident, filename, code)
    Ast(memberNode(EmptyAst(), "", "", ""))
      .withChild(
        Ast(newTypeDecl)
          .withChildren(annotationsAst)
      )

  }

  def astForTraitItemMacro(filename: String, parentFullname: String, traitItemMacro: TraitItemMacro): Ast = {
    val annotationsAst = traitItemMacro.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val macroInstance =
      Macro(traitItemMacro.path, traitItemMacro.delimiter, traitItemMacro.tokens)
    val macroAst = astForMacro(filename, parentFullname, macroInstance).withChildren(annotationsAst)

    Ast(memberNode(EmptyAst(), "", "", ""))
      .withChild(macroAst)
  }
}
