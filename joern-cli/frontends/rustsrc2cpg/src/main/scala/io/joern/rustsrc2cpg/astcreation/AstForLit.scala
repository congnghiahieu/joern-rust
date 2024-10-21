package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.collection.mutable.ListBuffer

trait AstForLit(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForLit(filename: String, parentFullname: String, litExprInstance: Lit): Ast = {
    if (litExprInstance.strLit.isDefined) {
      val value = litExprInstance.strLit.get
      val node  = literalNode(litExprInstance, value, "")
      Ast(node)
    } else if (litExprInstance.byteStrLit.isDefined) {
      val value = litExprInstance.byteStrLit.get
      val node  = literalNode(litExprInstance, value, "")
      Ast(node)
    } else if (litExprInstance.byteLit.isDefined) {
      val value = litExprInstance.byteLit.get
      val node  = literalNode(litExprInstance, value, "")
      Ast(node)
    } else if (litExprInstance.charLit.isDefined) {
      val value = litExprInstance.charLit.get
      val node  = literalNode(litExprInstance, value, "")
      Ast(node)
    } else if (litExprInstance.intLit.isDefined) {
      val value = litExprInstance.intLit.get
      val node  = literalNode(litExprInstance, value, "")
      Ast(node)
    } else if (litExprInstance.floatLit.isDefined) {
      val value = litExprInstance.floatLit.get
      val node  = literalNode(litExprInstance, value, "")
      Ast(node)
    } else if (litExprInstance.boolLit.isDefined) {
      val value = litExprInstance.boolLit.get
      val node  = literalNode(litExprInstance, value, "")
      Ast(node)
    } else if (litExprInstance.verbatimLit.isDefined) {
      val value = litExprInstance.verbatimLit.get
      val node  = literalNode(litExprInstance, value, "")
      Ast(node)
    } else {
      throw new RuntimeException("Unsupported literal type")
    }
  }
}
