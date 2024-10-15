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

trait AstForItem(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForItem(filename: String, parentFullname: String, itemInstance: Item): Ast = {
    if (itemInstance.constItem.isDefined) {
      return astForItemConst(filename, parentFullname, itemInstance.constItem.get)
    } else if (itemInstance.enumItem.isDefined) {
      return astForItemEnum(filename, parentFullname, itemInstance.enumItem.get)
    } else if (itemInstance.externCrateItem.isDefined) {
      return astForItemExternCrate(filename, parentFullname, itemInstance.externCrateItem.get)
    } else if (itemInstance.fnItem.isDefined) {
      return astForItemFn(filename, parentFullname, itemInstance.fnItem.get)
    } else if (itemInstance.foreignModItem.isDefined) {
      return astForItemForeignMod(filename, parentFullname, itemInstance.foreignModItem.get)
    } else if (itemInstance.implItem.isDefined) {
      return astForItemImpl(filename, parentFullname, itemInstance.implItem.get)
    } else if (itemInstance.macroItem.isDefined) {
      return astForItemMacro(filename, parentFullname, itemInstance.macroItem.get)
    } else if (itemInstance.modItem.isDefined) {
      return astForItemMod(filename, parentFullname, itemInstance.modItem.get)
    } else if (itemInstance.staticItem.isDefined) {
      return astForItemStatic(filename, parentFullname, itemInstance.staticItem.get)
    } else if (itemInstance.structItem.isDefined) {
      return astForItemStruct(filename, parentFullname, itemInstance.structItem.get)
    } else if (itemInstance.traitItem.isDefined) {
      return astForItemTrait(filename, parentFullname, itemInstance.traitItem.get)
    } else if (itemInstance.traitAliasItem.isDefined) {
      return astForItemTraitAlias(filename, parentFullname, itemInstance.traitAliasItem.get)
    } else if (itemInstance.typeItem.isDefined) {
      return astForItemType(filename, parentFullname, itemInstance.typeItem.get)
    } else if (itemInstance.unionItem.isDefined) {
      return astForItemUnion(filename, parentFullname, itemInstance.unionItem.get)
    } else if (itemInstance.useItem.isDefined) {
      return astForItemUse(filename, parentFullname, itemInstance.useItem.get)
    } else if (itemInstance.verbatimItem.isDefined) {
      return astForTokenStream(filename, parentFullname, itemInstance.verbatimItem.get)
    } else {
      throw new RuntimeException("Unknown item type")
    }
  }

  def astForItemConst(filename: String, parentFullname: String, itemConst: ItemConst): Ast = {
    val annotationsAst = itemConst.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode   = modifierForVisibility(filename, parentFullname, itemConst.vis)
    val genericsAst    = itemConst.generics.toList.flatMap(g => List(astForGenerics(filename, parentFullname, g)))

    val typeFullName = itemConst.ty match {
      case Some(ty) => typeFullnameForType(filename, parentFullname, ty)
      case None     => ""
    }
    val code         = s"const ${itemConst.ident}: ${typeFullName}"
    val newLocalNode = localNode(itemConst, itemConst.ident, code, typeFullName)

    Ast(newLocalNode)
    // .withChild(Ast(modifierNode))
    // .withChildren(annotationsAst)
    // .withChildren(genericsAst)
  }

  def astForItemEnum(filename: String, parentFullname: String, itemEnum: ItemEnum): Ast = {
    val annotationsAst = itemEnum.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val genericsAst    = itemEnum.generics.toList.flatMap(g => List(astForGenerics(filename, parentFullname, g)))

    val newEnumNode  = typeDeclNode(itemEnum, itemEnum.ident, "", filename, "")
    val modifierNode = modifierForVisibility(filename, parentFullname, itemEnum.vis)
    val variants     = itemEnum.variants.map(variant => astForVariant(filename, parentFullname, variant)).toList

    Ast(newEnumNode)
      .withChild(Ast(modifierNode))
      .withChildren(annotationsAst)
      .withChildren(variants)
      .withChildren(genericsAst)
  }

  def astForItemExternCrate(filename: String, parentFullname: String, itemExternCrate: ItemExternCrate): Ast = {
    val annotationsAst = itemExternCrate.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode   = modifierForVisibility(filename, parentFullname, itemExternCrate.vis)

    val importedEntity = itemExternCrate.ident
    val importedAs     = itemExternCrate.rename.getOrElse(itemExternCrate.ident)
    val code = itemExternCrate.rename match {
      case Some(rename) => s"${modifierNode.modifierType} extern crate ${importedEntity} as ${importedAs};"
      case None         => s"${modifierNode.modifierType} extern crate ${importedEntity};"
    }

    val importNode = newImportNode(code, importedEntity, importedAs, itemExternCrate)

    Ast(importNode)
    // .withChild(Ast(modifierNode))
    // .withChildren(annotationsAst)
  }

  def astForItemFn(filename: String, parentFullname: String, itemFn: ItemFn): Ast = {
    val annotationsAst = itemFn.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode   = modifierForVisibility(filename, parentFullname, itemFn.vis)

    val blockAst      = astForBlock(filename, parentFullname, itemFn.stmts)
    val newMethodNode = methodNode(itemFn, itemFn.ident, "", "", filename).isExternal(itemFn.stmts.isEmpty)
    val parameterIns  = itemFn.inputs.map(input => astForFnArg(filename, parentFullname, input)).toList
    val methodReturnTypeFullname = itemFn.output match {
      case Some(output) => typeFullnameForType(filename, parentFullname, output)
      case None         => ""
    }
    val methodRetNode = methodReturnNode(itemFn, methodReturnTypeFullname)

    methodAstWithAnnotations(newMethodNode, parameterIns, blockAst, methodRetNode, Seq(modifierNode), annotationsAst)
  }

  def astForItemForeignMod(filename: String, parentFullname: String, itemForeignMod: ItemForeignMod): Ast = {
    val annotationsAst = itemForeignMod.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val abiName  = nameForAbi(filename, parentFullname, itemForeignMod.abi)
    val isUnsafe = itemForeignMod.unsafe.getOrElse(false)
    val code = if (isUnsafe) { s"unsafe extern \"${abiName}\" {}" }
    else { s"extern {}" }
    val foreignItemAst = itemForeignMod.items.map(astForForeignItem(filename, parentFullname, _)).toList

    val foreignNamespaceBlock = NewNamespaceBlock()
      .name(abiName)
      .fullName(abiName)
      .filename(filename)
      .code(code)
    val foreignNamespaceAst = Ast(foreignNamespaceBlock)

    namespaceStack.push(foreignNamespaceAst.root.get)
    scope.pushNewScope(foreignNamespaceBlock)

    scope.popScope()
    namespaceStack.pop()

    foreignNamespaceAst
      .withChildren(annotationsAst)
      .withChildren(foreignItemAst)
  }

  def astForItemImpl(filename: String, parentFullname: String, itemImpl: ItemImpl): Ast = {
    val annotationsAst = itemImpl.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val genericsAst    = itemImpl.generics.toList.flatMap(g => List(astForGenerics(filename, parentFullname, g)))

    val implNode = typeDeclNode(itemImpl, "", "", filename, "")

    Ast(implNode)
      .withChildren(annotationsAst)
      .withChildren(genericsAst)
  }

  def astForItemMacro(filename: String, parentFullname: String, itemMacro: ItemMacro): Ast = {
    val annotationsAst = itemMacro.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val macroRustAst   = Macro(itemMacro.path, itemMacro.delimiter, itemMacro.tokens)
    astForMacro(filename, parentFullname, macroRustAst).withChildren(annotationsAst)
  }

  def astForItemMod(filename: String, parentFullname: String, itemMod: ItemMod): Ast = {
    val annotationsAst = itemMod.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode   = modifierForVisibility(filename, parentFullname, itemMod.vis)

    itemMod.semi match {
      case Some(true) => {
        var code = s"mod ${itemMod.ident};"
        if (itemMod.unsafe.getOrElse(false)) { code = s"unsafe ${code}" }
        if (modifierNode.modifierType == ModifierTypes.PUBLIC) { code = s"pub ${code}" }

        val importNode = newImportNode(code, itemMod.ident, itemMod.ident, itemMod)

        Ast(importNode)
      }
      // .withChild(Ast(modifierNode))
      // .withChildren(annotationsAst)
      case _ =>
        var code = s"mod ${itemMod.ident} {}"
        if (itemMod.unsafe.getOrElse(false)) { code = s"unsafe ${code}" }
        if (modifierNode.modifierType == ModifierTypes.PUBLIC) { code = s"pub ${code}" }

        val contentAst = itemMod.content.toList.flatMap(_.map(astForItem(filename, parentFullname, _)))

        val modNamespaceBlock = NewNamespaceBlock()
          .name(itemMod.ident)
          .fullName(itemMod.ident)
          .filename(filename)
          .code(code)
        val modNamespaceAst = Ast(modNamespaceBlock)

        namespaceStack.push(modNamespaceAst.root.get)
        scope.pushNewScope(modNamespaceBlock)

        scope.popScope()
        namespaceStack.pop()

        modNamespaceAst
          .withChild(Ast(modifierNode))
          .withChildren(annotationsAst)
          .withChildren(contentAst)
    }
  }

  def astForItemStatic(filename: String, parentFullname: String, itemStatic: ItemStatic): Ast = {
    val annotationsAst = itemStatic.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode   = modifierForVisibility(filename, parentFullname, itemStatic.vis)

    val typeFullname = itemStatic.ty.map(typeFullnameForType(filename, parentFullname, _)).getOrElse("")

    val isMut = itemStatic.mut.contains(StaticMutability.Mut)
    var code = if (isMut) { s"static mut ${itemStatic.ident}: ${typeFullname}" }
    else { s"static ${itemStatic.ident}: ${typeFullname}" }
    if (modifierNode.modifierType == ModifierTypes.PUBLIC) { code = s"pub ${code}" }

    val newLocalNode = localNode(itemStatic, itemStatic.ident, code, typeFullname)

    Ast(newLocalNode)
    // .withChild(Ast(modifierNode))
    // .withChildren(annotationsAst)
  }

  def astForItemStruct(filename: String, parentFullname: String, itemStruct: ItemStruct): Ast = {
    val annotationsAst = itemStruct.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode   = modifierForVisibility(filename, parentFullname, itemStruct.vis)
    val genericsAst    = itemStruct.generics.toList.flatMap(g => List(astForGenerics(filename, parentFullname, g)))

    var code = s"struct ${itemStruct.ident}"
    if (modifierNode.modifierType == ModifierTypes.PUBLIC) { code = s"pub ${code}" }
    val fieldsAst = itemStruct.fields.map(astForFields(filename, parentFullname, _)).getOrElse(Ast())

    val newItemStructNode = typeDeclNode(itemStruct, itemStruct.ident, itemStruct.ident, filename, code)

    Ast(newItemStructNode)
      .withChildren(annotationsAst)
      .withChildren(genericsAst)
      .withChild(fieldsAst)
  }

  def astForItemTrait(filename: String, parentFullname: String, itemTrait: ItemTrait): Ast = {
    val annotationsAst = itemTrait.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode   = modifierForVisibility(filename, parentFullname, itemTrait.vis)
    val genericsAst    = itemTrait.generics.toList.flatMap(g => List(astForGenerics(filename, parentFullname, g)))

    val isUnsafe = itemTrait.unsafe.getOrElse(false)
    var code = if (isUnsafe) { s"unsafe trait ${itemTrait.ident}" }
    else { s"trait ${itemTrait.ident}" }
    if (modifierNode.modifierType == ModifierTypes.PUBLIC) { code = s"pub ${code}" }

    val traitItemsAst = itemTrait.items.map(traitItem => astForTraitItem(filename, parentFullname, traitItem)).toList
    val supertraitsAst =
      itemTrait.supertraits.map(supertrait => astForTypeParamBound(filename, parentFullname, supertrait)).toList

    val newItemTraitNode = typeDeclNode(itemTrait, itemTrait.ident, itemTrait.ident, filename, code)

    Ast(newItemTraitNode)
      .withChildren(annotationsAst)
      .withChildren(genericsAst)
      .withChildren(traitItemsAst)
      .withChildren(supertraitsAst)
  }

  def astForItemTraitAlias(filename: String, parentFullname: String, itemTraitAlias: ItemTraitAlias): Ast = {
    val annotationsAst = itemTraitAlias.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode   = modifierForVisibility(filename, parentFullname, itemTraitAlias.vis)
    val genericsAst    = itemTraitAlias.generics.toList.flatMap(g => List(astForGenerics(filename, parentFullname, g)))

    var code = s"trait ${itemTraitAlias.ident}"
    if (modifierNode.modifierType == ModifierTypes.PUBLIC) { code = s"pub ${code}" }

    val boundsAst =
      itemTraitAlias.bounds.map(supertrait => astForTypeParamBound(filename, parentFullname, supertrait)).toList

    val newItemTraitAliasNode = typeDeclNode(itemTraitAlias, itemTraitAlias.ident, itemTraitAlias.ident, filename, code)

    Ast(newItemTraitAliasNode)
      .withChildren(annotationsAst)
      .withChildren(genericsAst)
      .withChildren(boundsAst)
  }

  def astForItemType(filename: String, parentFullname: String, itemType: ItemType): Ast = {
    val annotationsAst = itemType.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode   = modifierForVisibility(filename, parentFullname, itemType.vis)
    val genericsAst    = itemType.generics.toList.flatMap(g => List(astForGenerics(filename, parentFullname, g)))

    val typeFullname    = itemType.ty.map(typeFullnameForType(filename, parentFullname, _)).getOrElse("")
    val code            = s"type ${itemType.ident} = ${typeFullname}"
    val newItemTypeNode = typeDeclNode(itemType, itemType.ident, typeFullname, filename, code)

    Ast(newItemTypeNode)
      .withChildren(annotationsAst)
      .withChildren(genericsAst)
  }

  def astForItemUnion(filename: String, parentFullname: String, itemUnion: ItemUnion): Ast = {
    val annotationsAst = itemUnion.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode   = modifierForVisibility(filename, parentFullname, itemUnion.vis)
    val genericsAst    = itemUnion.generics.toList.flatMap(g => List(astForGenerics(filename, parentFullname, g)))

    val code            = s"union ${itemUnion.ident} {}"
    val newItemTypeNode = typeDeclNode(itemUnion, itemUnion.ident, itemUnion.ident, filename, code)

    Ast(newItemTypeNode)
      .withChildren(annotationsAst)
      .withChildren(genericsAst)
  }

  def astForItemUse(filename: String, parentFullname: String, itemUse: ItemUse): Ast = {
    val annotationsAst = itemUse.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val modifierNode   = modifierForVisibility(filename, parentFullname, itemUse.vis)

    val importedEntity = ""
    val importedAs     = ""
    val code           = ""

    val importNode = newImportNode(code, importedEntity, importedAs, itemUse)

    Ast(importNode)
    // .withChild(Ast(modifierNode))
    // .withChildren(annotationsAst)
  }
}
