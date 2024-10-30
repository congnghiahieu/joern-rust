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

trait AstForRangeLimits(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForRangeLimits(filename: String, parentFullname: String, rangeLimitsIntance: RangeLimits): Ast = {
    val code         = rangeLimitsIntance.toString
    val typeFullname = rangeLimitsIntance.toString
    val node         = literalNode(UnknownAst(), code, typeFullname)
    Ast(node)
  }
}
