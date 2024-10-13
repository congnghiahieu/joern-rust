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
trait AstForGenericParam(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForGenericParam(filename: String, parentFullname: String, genericParamInstance: GenericParam): Ast = {
    if (genericParamInstance.lifetimeGenericParam.isDefined) {
      astForLifetimeGenericParam(filename, parentFullname, genericParamInstance.lifetimeGenericParam.get)
    } else if (genericParamInstance.typeGenericParam.isDefined) {
      astForTypeGenericParam(filename, parentFullname, genericParamInstance.typeGenericParam.get)
    } else if (genericParamInstance.constGenericParam.isDefined) {
      astForConstGenericParam(filename, parentFullname, genericParamInstance.constGenericParam.get)
    } else {
      throw new IllegalArgumentException("Unsupported generic param type")
    }
  }

  def astForLifetimeGenericParam(
    filename: String,
    parentFullname: String,
    lifetimeParamInstance: LifetimeParam
  ): Ast = {
    val typeParameterNode = NewTypeParameter()
    Ast(typeParameterNode)
  }

  def astForTypeGenericParam(filename: String, parentFullname: String, typeParamInstance: TypeParam): Ast = {
    val typeParameterNode = NewTypeParameter()
    Ast(typeParameterNode)
  }

  def astForConstGenericParam(filename: String, parentFullname: String, constParamInstance: ConstParam): Ast = {
    val typeParameterNode = NewTypeParameter()
    Ast(typeParameterNode)
  }
}