package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.UnknownAst
import io.joern.rustsrc2cpg.ast.Member
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

trait AstForMember(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForMember(filename: String, parentFullname: String, member: Member): Ast = {
    if (member.named.isDefined) {
      val name      = member.named.get
      val identNode = fieldIdentifierNode(UnknownAst(), name, name)
      Ast(identNode)
    } else if (member.unnamed.isDefined) {
      val name      = member.unnamed.get.toString
      val indexNode = fieldIdentifierNode(UnknownAst(), name, name)
      Ast(indexNode)
    } else {
      throw new IllegalArgumentException("Unsupported member type")
    }
  }
}
trait CodeForMember(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def codeForMember(filename: String, parentFullname: String, member: Member): String = {
    if (member.named.isDefined) {
      member.named.get
    } else if (member.unnamed.isDefined) {
      member.unnamed.get.toString
    } else {
      throw new IllegalArgumentException("Unsupported member type")
    }
  }
}
