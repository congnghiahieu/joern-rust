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
trait AstForImplItem(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForImplItem(filename: String, parentFullname: String, implItemInstance: ImplItem): Ast = {
    if (implItemInstance.constImplItem.isDefined) {
      astForImplItemConst(filename, parentFullname, implItemInstance.constImplItem.get)
    } else if (implItemInstance.fnImplItem.isDefined) {
      astForImplItemFn(filename, parentFullname, implItemInstance.fnImplItem.get)
    } else if (implItemInstance.typeImplItem.isDefined) {
      astForImplItemType(filename, parentFullname, implItemInstance.typeImplItem.get)
    } else if (implItemInstance.macroImplItem.isDefined) {
      astForImplItemMacro(filename, parentFullname, implItemInstance.macroImplItem.get)
    } else if (implItemInstance.verbatimImplItem.isDefined) {
      astForTokenStream(filename, parentFullname, implItemInstance.verbatimImplItem.get)
    } else {
      throw new IllegalArgumentException("Unsupported impl item type")
    }
  }

  def astForImplItemConst(filename: String, parentFullname: String, constImplItemInstance: ImplItemConst): Ast = {
    val annotationsAst = constImplItemInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val node = localNode(constImplItemInstance, "", "", "")
    val localAst = Ast(node)
      .withChildren(annotationsAst)

    Ast(memberNode(EmptyAst(), "", "", ""))
      .withChild(localAst)
  }

  def astForImplItemFn(filename: String, parentFullname: String, fnImplItemInstance: ImplItemFn): Ast = {
    val annotationsAst = fnImplItemInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val modifierNode = modifierForVisibility(filename, parentFullname, fnImplItemInstance.vis)
    val blockAst     = astForBlock(filename, parentFullname, fnImplItemInstance.stmts)
    val newMethodNode = methodNode(fnImplItemInstance, fnImplItemInstance.ident, "", "", filename).isExternal(
      fnImplItemInstance.stmts.isEmpty
    )
    val parameterIns = fnImplItemInstance.inputs.map(input => astForFnArg(filename, parentFullname, input)).toList
    val methodReturnTypeFullname = fnImplItemInstance.output match {
      case Some(output) => typeFullnameForType(filename, parentFullname, output)
      case None         => ""
    }
    val methodRetNode = methodReturnNode(fnImplItemInstance, methodReturnTypeFullname)
    val methodAst =
      methodAstWithAnnotations(newMethodNode, parameterIns, blockAst, methodRetNode, Seq(modifierNode), annotationsAst)
    Ast(memberNode(EmptyAst(), "", "", "")).withChild(methodAst)
  }

  def astForImplItemType(filename: String, parentFullname: String, typeImplItemInstance: ImplItemType): Ast = {
    val annotationsAst = typeImplItemInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val modifierNode = modifierForVisibility(filename, parentFullname, typeImplItemInstance.vis)
    val genericsAst =
      typeImplItemInstance.generics.toList.flatMap(g => List(astForGenerics(filename, parentFullname, g)))
    val typeFullname = typeImplItemInstance.ty.map(typeFullnameForType(filename, parentFullname, _)).getOrElse("")
    val code         = s"type ${typeImplItemInstance.ident} = ${typeFullname}"

    val newTypeImplItemInstanceNode =
      typeDeclNode(typeImplItemInstance, typeImplItemInstance.ident, typeFullname, filename, code)
    val implItemTypeAst = Ast(newTypeImplItemInstanceNode)
      .withChildren(annotationsAst)
      .withChildren(genericsAst)
    Ast(memberNode(EmptyAst(), "", "", "")).withChild(implItemTypeAst)
  }

  def astForImplItemMacro(filename: String, parentFullname: String, macroImplItemInstance: ImplItemMacro): Ast = {
    val annotationsAst = macroImplItemInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val marcoInstance =
      Macro(macroImplItemInstance.path, macroImplItemInstance.delimiter, macroImplItemInstance.tokens)
    val macroAst = astForMacro(filename, parentFullname, marcoInstance).withChildren(annotationsAst)

    Ast(memberNode(EmptyAst(), "", "", ""))
      .withChild(macroAst)
  }

  def astForQself(filename: String, parentFullname: String, qselfInstance: QSelf): Ast = {
    val qselfAst = NewTypeParameter()
    Ast(qselfAst)
  }
}
