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
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import scala.collection.mutable.ListBuffer

trait AstForStmt(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>

  def astForBlock(filename: String, parentFullname: String, blockInstance: Block): Ast = {
    val node     = blockNode(UnknownAst(), "{}", "")
    val stmtsAst = blockInstance.map(astForStmt(filename, parentFullname, _)).toList
    blockAst(node, stmtsAst)
  }

  def astForStmt(filename: String, parentFullname: String, stmtInstance: Stmt): Ast = {
    if (stmtInstance.letStmt.isDefined) {
      return astForLocal(filename, parentFullname, stmtInstance.letStmt.get)
    } else if (stmtInstance.itemStmt.isDefined) {
      return astForItem(filename, parentFullname, stmtInstance.itemStmt.get)
    } else if (stmtInstance.exprStmt.isDefined) {
      return astForExpr(filename, parentFullname, stmtInstance.exprStmt.get._1)
    } else if (stmtInstance.macroStmt.isDefined) {
      return astForMacroStmt(filename, parentFullname, stmtInstance.macroStmt.get)
    } else {
      throw new RuntimeException(s"Unknown fnArg type: $stmtInstance")
    }
  }

  def astForMacroStmt(filename: String, parentFullname: String, macroStmtInstance: StmtMacro): Ast = {
    val annotationsAst = macroStmtInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val macroRustAst = Macro(macroStmtInstance.path, macroStmtInstance.delimiter, macroStmtInstance.tokens)
    astForMacro(filename, parentFullname, macroRustAst).withChildren(annotationsAst)
  }

  def astForLocal(filename: String, parentFullname: String, localInstance: Local): Ast = {
    val annotationsAst = localInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val localInitAst = localInstance.init match {
      case Some(init) => astForLocalInit(filename, parentFullname, init)
      case None       => Ast()
    }

    val name = localInstance.pat match {
      case Some(pat) => codeForPat(filename, parentFullname, pat)
      case None      => Defines.Unknown
    }
    val code = s"let $name"
    val node = localNode(localInstance, name, code, "")
    val patAst = localInstance.pat match {
      case Some(pat) => astForPat(filename, parentFullname, pat)
      case None      => Ast()
    }
    val wrapperAst = Ast(unknownNode(UnknownAst(), ""))
      .withChild(Ast(node))
      .withChild(patAst)

    Ast(unknownNode(localInstance, ""))
      .withChild(wrapperAst)
      .withChild(localInitAst)
      .withChildren(annotationsAst)
  }

  def astForLocalInit(filename: String, parentFullname: String, localInitInstance: LocalInit): Ast = {
    val exprAst = localInitInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }
    val divergeAst = localInitInstance.diverge match {
      case Some(diverge) => {
        val divergeExpr = astForExpr(filename, parentFullname, diverge)
        val elseNode    = controlStructureNode(ExprElse(), ControlStructureTypes.ELSE, "")
        controlStructureAst(elseNode, None).withChild(divergeExpr)
      }
      case None => Ast()
    }

    Ast(unknownNode(localInitInstance, ""))
      .withChild(exprAst)
      .withChild(divergeAst)
  }
}
