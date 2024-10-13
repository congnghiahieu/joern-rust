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
    val binOpAst = NewLiteral().code(binOpInstance.toString)
    Ast(binOpAst)
  }

  def astForUnOp(filename: String, parentFullname: String, unOpInstance: UnOp): Ast = {
    val unOpAst = NewLiteral().code(unOpInstance.toString)
    Ast(unOpAst)
  }
}
