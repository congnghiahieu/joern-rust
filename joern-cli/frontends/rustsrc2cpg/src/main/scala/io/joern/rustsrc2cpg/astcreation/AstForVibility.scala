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

trait AstForVisibility(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>

  def astForVisibility(filename: String, parentFullname: String, visibilityInstance: Option[Visibility]): Ast = {
    Ast(modifierForVisibility(filename, parentFullname, visibilityInstance))
  }

  def modifierForVisibility(
    filename: String,
    parentFullname: String,
    visibilityInstance: Option[Visibility]
  ): NewModifier = {
    if (!visibilityInstance.isDefined) {
      return newModifierNode(ModifierTypes.PRIVATE)
    }

    visibilityInstance.get match {
      case visibilityString: VisibilityString => {
        visibilityString match {
          case VisibilityString.VisibilityPublic =>
            return newModifierNode(ModifierTypes.PUBLIC)
          case VisibilityString.VisibilityInherited =>
            return newModifierNode(ModifierTypes.PUBLIC)
        }
      }
      case visibilityOther: VisibilityOther => {
        if (!visibilityOther.restricted.isDefined) {
          return newModifierNode(ModifierTypes.PRIVATE)
        }

        return newModifierNode(ModifierTypes.PUBLIC)
      }
    }
  }
}
