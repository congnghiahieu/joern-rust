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
    val annotationsAst = fnForeignItemInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val modifierNode = modifierForVisibility(filename, parentFullname, fnForeignItemInstance.vis)

    val bodyAst       = blockAst(blockNode(fnForeignItemInstance, "{}", ""))
    val newMethodNode = methodNode(fnForeignItemInstance, fnForeignItemInstance.ident, "", "", filename)
    val parameterIns  = fnForeignItemInstance.inputs.map(astForFnArg(filename, parentFullname, _)).toList
    val methodReturnTypeFullname = fnForeignItemInstance.output match {
      case Some(output) => typeFullnameForType(filename, parentFullname, output)
      case None         => Defines.Unknown
    }
    val methodRetNode = methodReturnNode(fnForeignItemInstance, methodReturnTypeFullname)

    methodAstWithAnnotations(newMethodNode, parameterIns, bodyAst, methodRetNode, Seq(modifierNode), annotationsAst)
  }

  def astForForeignItemStatic(
    filename: String,
    parentFullname: String,
    staticForeignItemInstance: ForeignItemStatic
  ): Ast = {
    val annotationsAst = staticForeignItemInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val modifierNode = modifierForVisibility(filename, parentFullname, staticForeignItemInstance.vis)

    val typeFullname = staticForeignItemInstance.ty.map(typeFullnameForType(filename, parentFullname, _)).getOrElse("")

    val isMut = staticForeignItemInstance.mut.contains(StaticMutability.Mut)
    var code = if (isMut) { s"static mut ${staticForeignItemInstance.ident}: ${typeFullname}" }
    else { s"static ${staticForeignItemInstance.ident}: ${typeFullname}" }
    if (modifierNode.modifierType == ModifierTypes.PUBLIC) { code = s"pub ${code}" }

    val newLocalNode = localNode(staticForeignItemInstance, staticForeignItemInstance.ident, code, typeFullname)

    val staticAst = Ast(newLocalNode)

    Ast(unknownNode(staticForeignItemInstance, ""))
      .withChild(staticAst)
      .withChild(Ast(modifierNode))
      .withChildren(annotationsAst)
  }

  def astForForeignItemType(filename: String, parentFullname: String, typeForeignItemInstance: ForeignItemType): Ast = {
    val annotationsAst = typeForeignItemInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val modifierNode = modifierForVisibility(filename, parentFullname, typeForeignItemInstance.vis)
    val genericsAst =
      typeForeignItemInstance.generics match {
        case Some(generics) => astForGenerics(filename, parentFullname, generics)
        case None           => Ast()
      }

    val newItemTypeNode =
      typeDeclNode(typeForeignItemInstance, typeForeignItemInstance.ident, "", filename, "")

    Ast(newItemTypeNode)
      .withChild(genericsAst)
      .withChildren(annotationsAst)
  }

  def astForForeignItemMacro(
    filename: String,
    parentFullname: String,
    macroForeignItemInstance: ForeignItemMacro
  ): Ast = {
    val annotationsAst = macroForeignItemInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val macroRustAst =
      Macro(macroForeignItemInstance.path, macroForeignItemInstance.delimiter, macroForeignItemInstance.tokens)
    astForMacro(filename, parentFullname, macroRustAst)
      .withChildren(annotationsAst)
  }
}
