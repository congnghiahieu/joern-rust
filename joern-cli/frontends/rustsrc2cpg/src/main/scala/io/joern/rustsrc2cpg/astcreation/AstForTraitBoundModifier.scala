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

trait AstForTraitBoundModifier(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForTraitBound(filename: String, parentFullname: String, traitBound: TraitBound): Ast = {
    val node = NewTypeParameter()
    Ast(node)
  }

  def codeForTraitBound(filename: String, parentFullname: String, traitBound: TraitBound): String = {
    val typeFullname = traitBound.path match {
      case Some(path) => typeFullnameForPath(filename, parentFullname, path)
      case None       => ""
    }

    traitBound.modifier match {
      case Some(TraitBoundModifier.Maybe) => s"?$typeFullname"
      case _                              => ""
    }
  }
}
