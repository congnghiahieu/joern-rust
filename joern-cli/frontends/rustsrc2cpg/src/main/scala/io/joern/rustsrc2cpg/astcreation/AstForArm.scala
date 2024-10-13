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
    val armNode =
      NewControlStructure().controlStructureType(ControlStructureTypes.MATCH).parserTypeName(classOf[Arm].getSimpleName)

    Ast(armNode)
  }

  def astForLabel(filename: String, parentFullname: String, label: Label): Ast = {
    val labelNode = NewJumpLabel()

    Ast(labelNode)
  }
}
