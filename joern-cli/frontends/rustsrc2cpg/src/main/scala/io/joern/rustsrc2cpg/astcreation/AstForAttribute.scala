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

trait AstForAttribute(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForAttribute(filename: String, parentFullname: String, attributeInstance: Attribute): Ast = {
    // var style = attributeInstance.style.getOrElse(AttrStyle.Outer)
    // val (attributeTypeFullname, input, attributeCode) = attributeInstance.meta match {
    //   case Some(value) => {
    //     val node = astForMeta(filename, parentFullname, value)
    //     node
    //   }
    //   case None => ("", "", "")
    // }

    // val code = style match {
    //   case AttrStyle.Outer => s"#[$attributeTypeFullname($input)]"
    //   case AttrStyle.Inner => s"#![$attributeTypeFullname($input)]"
    // }
    // val node = annotationNode(attributeInstance, code, attributeTypeFullname, attributeTypeFullname)

    val annotationNode = NewAnnotation()
    Ast(annotationNode)
  }
}
