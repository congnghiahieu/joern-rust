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

trait AstForTypeParamBound(implicit schemaValidationMode: ValidationMode) {
  this: AstCreator =>
  def astForTypeParamBound(filename: String, parentFullname: String, typeParamBoundInstance: TypeParamBound): Ast = {
    if (typeParamBoundInstance.traitParamBound.isDefined) {
      return astForTraitBound(filename, parentFullname, typeParamBoundInstance.traitParamBound.get)
    } else if (typeParamBoundInstance.lifetimeParamBound.isDefined) {
      return astForLifetimeAsParam(filename, parentFullname, typeParamBoundInstance.lifetimeParamBound.get)
    } else if (typeParamBoundInstance.verbatimParamBound.isDefined) {
      return astForTokenStream(filename, parentFullname, typeParamBoundInstance.verbatimParamBound.get)
    } else {
      throw new RuntimeException("Unknown type parameter bound type")
    }
  }
}

trait CodeForTypeParamBound(implicit schemaValidationMode: ValidationMode) {
  this: AstCreator =>
  def codeForTypeParamBound(
    filename: String,
    parentFullname: String,
    typeParamBoundInstance: TypeParamBound
  ): String = {
    if (typeParamBoundInstance.traitParamBound.isDefined) {
      return codeForTraitBound(filename, parentFullname, typeParamBoundInstance.traitParamBound.get)
    } else if (typeParamBoundInstance.lifetimeParamBound.isDefined) {
      return codeForLifetime(filename, parentFullname, typeParamBoundInstance.lifetimeParamBound.get)
    } else if (typeParamBoundInstance.verbatimParamBound.isDefined) {
      return codeForTokenStream(filename, parentFullname, typeParamBoundInstance.verbatimParamBound.get)
    } else {
      throw new RuntimeException("Unknown type parameter bound type")
    }
  }
}
