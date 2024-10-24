package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
import io.joern.rustsrc2cpg.ast.Block
import io.joern.rustsrc2cpg.ast.Local
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.utils.NodeBuilders.newThisParameterNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.nodes.Block.PropertyDefaults as BlockDefaults

import scala.collection.mutable.ListBuffer

trait AstForStmt(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>

  def astForBlock(filename: String, parentFullname: String, blockInstance: Block): Ast = {
    val node     = blockNode(EmptyAst())
    val stmtsAst = blockInstance.map(astForStmt(filename, parentFullname, _)).toList
    blockAst(node, stmtsAst)
  }

  def astForStmt(filename: String, parentFullname: String, stmtInstance: Stmt): Ast = {
    if (stmtInstance.letStmt.isDefined) {
      return astForLetStmt(filename, parentFullname, stmtInstance.letStmt.get)
    } else if (stmtInstance.itemStmt.isDefined) {
      return astForItemStmt(filename, parentFullname, stmtInstance.itemStmt.get)
    } else if (stmtInstance.exprStmt.isDefined) {
      return astForExprStmt(filename, parentFullname, stmtInstance.exprStmt.get)
    } else if (stmtInstance.macroStmt.isDefined) {
      return astForMacroStmt(filename, parentFullname, stmtInstance.macroStmt.get)
    } else {
      throw new RuntimeException(s"Unknown fnArg type: $stmtInstance")
    }
  }

  def astForLetStmt(filename: String, parentFullname: String, letStmtInstance: Local): Ast = {
    astForLocal(filename, parentFullname, letStmtInstance)
  }
  def astForItemStmt(filename: String, parentFullname: String, itemStmtInstance: Item): Ast = {
    astForItem(filename, parentFullname, itemStmtInstance)
  }
  def astForExprStmt(filename: String, parentFullname: String, exprStmtInstance: (Expr, Boolean)): Ast = {
    astForExpr(filename, parentFullname, exprStmtInstance._1)
  }
  def astForMacroStmt(filename: String, parentFullname: String, macroStmtInstance: StmtMacro): Ast = {
    val annotationsAst = macroStmtInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val macroRustAst = Macro(macroStmtInstance.path, macroStmtInstance.delimiter, macroStmtInstance.tokens)
    astForMacro(filename, parentFullname, macroRustAst).withChildren(annotationsAst)
  }

  def astForLocal(filename: String, parentFullname: String, localInstance: Local): Ast = {
    val annotationsAst = localInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val patAst = localInstance.pat match {
      case Some(pat) => astForPat(filename, parentFullname, pat)
      case None      => Ast()
    }

    val name = localInstance.pat match {
      case Some(pat) => codeForPat(filename, parentFullname, pat)
      case None      => ""
    }
    val code = s"let $name"
    val node = localNode(localInstance, name, code, name)

    Ast(node)
    // .withChild(patAst)
    // .withChildren(annotationsAst)
  }

  def astForLocalInit(filename: String, parentFullname: String, localInitInstance: LocalInit): Ast = {
    val exprAst    = localInitInstance.expr.map(astForExpr(filename, parentFullname, _)).toList
    val divergeAst = localInitInstance.diverge.map(astForExpr(filename, parentFullname, _)).toList

    val node = unknownNode(localInitInstance, "")
    Ast(node)
      .withChildren(exprAst)
      .withChildren(divergeAst)
  }
}
