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

trait AstForForeignItem(implicit schemaValidationMode: ValidationMode) {
  this: AstCreator =>
  def astForForeignItem(filename: String, parentFullname: String, foreignItemInstance: ForeignItem): Ast = {
    if (foreignItemInstance.fnForeignItem.isDefined) {
      astForForeignItemFn(filename, parentFullname, foreignItemInstance.fnForeignItem.get)
    } else if (foreignItemInstance.staticForeignItem.isDefined) {
      astForForeignItemStatic(filename, parentFullname, foreignItemInstance.staticForeignItem.get)
    } else if (foreignItemInstance.typeForeignItem.isDefined) {
      astForForeignItemType(filename, parentFullname, foreignItemInstance.typeForeignItem.get)
    } else if (foreignItemInstance.macroForeignItem.isDefined) {
      astForForeignItemMacro(filename, parentFullname, foreignItemInstance.macroForeignItem.get)
    } else if (foreignItemInstance.verbatimForeignItem.isDefined) {
      astForTokenStream(filename, parentFullname, foreignItemInstance.verbatimForeignItem.get)
    } else {
      throw new IllegalArgumentException("Unsupported foreign item type")
    }
  }

  def astForForeignItemFn(filename: String, parentFullname: String, fnForeignItemInstance: ForeignItemFn): Ast = {
    val annotationsAst = fnForeignItemInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode   = modifierForVisibility(filename, parentFullname, fnForeignItemInstance.vis)

    val bodyAst       = blockAst(blockNode(fnForeignItemInstance, "", filename))
    val newMethodNode = methodNode(fnForeignItemInstance, fnForeignItemInstance.ident, "", "", filename)
    val parameterIns  = fnForeignItemInstance.inputs.map(input => astForFnArg(filename, parentFullname, input)).toList
    val methodReturnTypeFullname = fnForeignItemInstance.output match {
      case Some(output) => typeFullnameForType(filename, parentFullname, output)
      case None         => ""
    }
    val methodRetNode = methodReturnNode(fnForeignItemInstance, methodReturnTypeFullname)

    methodAstWithAnnotations(newMethodNode, parameterIns, bodyAst, methodRetNode, Seq(modifierNode), annotationsAst)
  }

  def astForForeignItemStatic(
    filename: String,
    parentFullname: String,
    staticForeignItemInstance: ForeignItemStatic
  ): Ast = {
    val annotationsAst =
      staticForeignItemInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode = modifierForVisibility(filename, parentFullname, staticForeignItemInstance.vis)

    val typeFullname = staticForeignItemInstance.ty.map(typeFullnameForType(filename, parentFullname, _)).getOrElse("")

    val isMut = staticForeignItemInstance.mut.contains(StaticMutability.Mut)
    var code = if (isMut) { s"static mut ${staticForeignItemInstance.ident}: ${typeFullname}" }
    else { s"static ${staticForeignItemInstance.ident}: ${typeFullname}" }
    if (modifierNode.modifierType == ModifierTypes.PUBLIC) { code = s"pub ${code}" }

    val newLocalNode = localNode(staticForeignItemInstance, staticForeignItemInstance.ident, code, typeFullname)

    val staticAst = Ast(newLocalNode)
    // .withChild(Ast(modifierNode))
    // .withChildren(annotationsAst)

    staticAst
  }

  def astForForeignItemType(filename: String, parentFullname: String, typeForeignItemInstance: ForeignItemType): Ast = {
    val annotationsAst =
      typeForeignItemInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode = modifierForVisibility(filename, parentFullname, typeForeignItemInstance.vis)
    val genericsAst =
      typeForeignItemInstance.generics.toList.flatMap(g => List(astForGenerics(filename, parentFullname, g)))

    val newItemTypeNode =
      typeDeclNode(typeForeignItemInstance, typeForeignItemInstance.ident, "", filename, "")

    val typeAst = Ast(newItemTypeNode)
      .withChildren(annotationsAst)
      .withChildren(genericsAst)
    typeAst
  }

  def astForForeignItemMacro(
    filename: String,
    parentFullname: String,
    macroForeignItemInstance: ForeignItemMacro
  ): Ast = {
    val macroRustAst =
      Macro(macroForeignItemInstance.path, macroForeignItemInstance.delimiter, macroForeignItemInstance.tokens)
    val macroAst = astForMacro(filename, parentFullname, macroRustAst)

    macroAst
  }
}
