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

trait AstForFnArg(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>

  def astForFnArg(filename: String, parentFullname: String, fnArg: FnArg): Ast = {
    Ast(parameterInForFnArg(filename, parentFullname, fnArg))
  }

  def parameterInForFnArg(filename: String, parentFullname: String, fnArg: FnArg): NewMethodParameterIn = {
    if (fnArg.receiverFnArg.isDefined) {
      return parameterInForReceiver(filename, parentFullname, fnArg.receiverFnArg.get)
    } else if (fnArg.typedFnArg.isDefined) {
      return parameterInForPatType(filename, parentFullname, fnArg.typedFnArg.get)
    } else {
      throw new RuntimeException(s"Unknown fnArg type: $fnArg")
    }
  }

  def parameterInForPatType(
    filename: String,
    parentFullname: String,
    patTypeInstance: PatType
  ): NewMethodParameterIn = {
    parameterInNode(patTypeInstance, "", "", 0, false, EvaluationStrategies.BY_VALUE, "")
  }

  def parameterInForReceiver(
    filename: String,
    parentFullname: String,
    receiverInstance: Receiver
  ): NewMethodParameterIn = {

    val isMut       = receiverInstance.mut.isDefined && receiverInstance.mut.get
    val isReference = receiverInstance.ref.isDefined && receiverInstance.ref.get
    val evaluationStrategy = if (isMut) { EvaluationStrategies.BY_REFERENCE }
    else { EvaluationStrategies.BY_VALUE }

    val code = s"${if (isReference) "&" else ""}${if (isMut) "mut " else ""}self"

    // newThisParameterNode(name = "self", code = code, typeFullName = "Self", evaluationStrategy = evaluationStrategy)
    parameterInNode(receiverInstance, "", "", 0, false, EvaluationStrategies.BY_VALUE, "")
  }
}
