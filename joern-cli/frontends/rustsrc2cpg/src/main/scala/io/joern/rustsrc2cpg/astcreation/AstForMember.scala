package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.EmptyAst
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
      val identNode = fieldIdentifierNode(EmptyAst(), member.named.get, member.named.get)
      Ast(identNode)
    } else if (member.unnamed.isDefined) {
      val indexNode = fieldIdentifierNode(EmptyAst(), member.unnamed.get.toString, member.unnamed.get.toString)
      Ast(indexNode)
    } else {
      throw new IllegalArgumentException("Unsupported member type")
    }
  }
}
