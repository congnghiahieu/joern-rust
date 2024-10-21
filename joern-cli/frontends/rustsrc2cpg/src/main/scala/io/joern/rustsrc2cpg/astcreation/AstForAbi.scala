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

trait AstForAbi(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  val DEFAULT_ABI_NAME = "C"

  def astForAbi(filename: String, parentFullname: String, abi: Abi): Ast = {
    val abiName = nameForAbi(filename, parentFullname, Some(abi))
    val code    = s"extern \"${abiName}\" { }"

    val abiNamespace = NewNamespaceBlock()
      .name(abiName)
      .fullName(abiName)
      .filename(filename)
      .code(code)
    Ast(abiNamespace)
  }

  def nameForAbi(filename: String, parentFullname: String, abi: Option[Abi]): String = {
    abi match {
      case Some(abi) => {
        abi.name match {
          case Some(name) => name
          case None       => DEFAULT_ABI_NAME
        }
      }
      case None => DEFAULT_ABI_NAME
    }
  }
}
