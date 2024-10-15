package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.utils.NodeBuilders.newThisParameterNode
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.collection.mutable.ListBuffer

trait AstForArm(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForArm(filename: String, parentFullname: String, arm: Arm): Ast = {
    val code    = ""
    val armNode = controlStructureNode(arm, ControlStructureTypes.MATCH, code)

    var conditionAst = blockAst(blockNode(EmptyAst()))
    conditionAst = arm.pat match {
      case Some(pat) => conditionAst.withChild(astForPat(filename, armNode.parserTypeName, pat))
      case None      => conditionAst
    }
    conditionAst = arm.guard match {
      case Some(guard) => conditionAst.withChild(astForExpr(filename, armNode.parserTypeName, guard))
      case None        => conditionAst
    }
    val bodyAst = arm.body.toList.map(astForExpr(filename, armNode.parserTypeName, _))

    controlStructureAst(armNode, Some(conditionAst), bodyAst, false)
  }

  def astForLabel(filename: String, parentFullname: String, label: Label): Ast = {
    val code      = s"'${label}:"
    val labelNode = jumpTargetNode(EmptyAst(), label, code, Some(classOf[Label].getSimpleName))
    Ast(labelNode)
  }
}
