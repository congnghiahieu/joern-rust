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
    val newLocal = NewLocal()
    Ast(NewMember()).withChild(Ast(newLocal))
  }

  def astForTraitItemFn(filename: String, parentFullname: String, traitItemFn: TraitItemFn): Ast = {
    val newMethodAst = Ast(NewMethod()).withChild(Ast(NewMethodReturn()))
    Ast(NewMember()).withChild(newMethodAst)
  }

  def astForTraitItemType(filename: String, parentFullname: String, traitItemType: TraitItemType): Ast = {
    val newTypeDecl = NewTypeDecl()
    Ast(NewMember()).withChild(Ast(newTypeDecl))
  }

  def astForTraitItemMacro(filename: String, parentFullname: String, traitItemMacro: TraitItemMacro): Ast = {
    val newCall = NewCall()
    Ast(NewMember()).withChild(Ast(newCall))
  }
}
