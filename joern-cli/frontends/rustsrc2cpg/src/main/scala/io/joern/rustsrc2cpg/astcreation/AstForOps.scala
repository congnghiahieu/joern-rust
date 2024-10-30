package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
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

import scala.collection.mutable.ListBuffer

trait AstForOps(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForBinOp(filename: String, parentFullname: String, binOpInstance: BinOp): Ast = {
    val code         = binOpInstance.toString
    val typeFullname = binOpInstance.toString
    val binOpAst     = literalNode(UnknownAst(), code, typeFullname)
    Ast(binOpAst)
  }

  def astForUnOp(filename: String, parentFullname: String, unOpInstance: UnOp): Ast = {
    val code         = unOpInstance.toString
    val typeFullname = unOpInstance.toString
    val unOpAst      = literalNode(UnknownAst(), code, typeFullname)
    Ast(unOpAst)
  }
}
