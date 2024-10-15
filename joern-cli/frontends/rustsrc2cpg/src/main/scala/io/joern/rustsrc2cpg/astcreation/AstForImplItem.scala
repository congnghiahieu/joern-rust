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
    val newLocal = localNode(constImplItemInstance, "", "", "")
    Ast(memberNode(EmptyAst(), "", "", "")).withChild(Ast(newLocal))
  }

  def astForImplItemFn(filename: String, parentFullname: String, fnImplItemInstance: ImplItemFn): Ast = {
    val newMethodAst = Ast(NewMethod().filename(filename)).withChild(Ast(NewMethodReturn()))
    Ast(memberNode(EmptyAst(), "", "", "")).withChild(newMethodAst)
  }

  def astForImplItemType(filename: String, parentFullname: String, typeImplItemInstance: ImplItemType): Ast = {
    val newTypeDecl = NewTypeDecl().filename(filename)
    Ast(memberNode(EmptyAst(), "", "", "")).withChild(Ast(newTypeDecl))
  }

  def astForImplItemMacro(filename: String, parentFullname: String, macroImplItemInstance: ImplItemMacro): Ast = {
    val macroRustAst =
      Macro(macroImplItemInstance.path, macroImplItemInstance.delimiter, macroImplItemInstance.tokens)
    Ast(memberNode(EmptyAst(), "", "", "")).withChild(astForMacro(filename, parentFullname, macroRustAst))
  }

  def astForQself(filename: String, parentFullname: String, qselfInstance: QSelf): Ast = {
    val qselfAst = NewTypeParameter()
    Ast(qselfAst)
  }
}
