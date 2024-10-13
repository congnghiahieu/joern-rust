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

  def astForVisibility(filename: String, parentFullname: String, visibilityInstance: Visibility): Ast = {
    visibilityInstance match {
      case visibilityString: VisibilityString => {
        if (visibilityString == VisibilityString.VisibilityPublic) {
          return Ast(newModifierNode(ModifierTypes.PUBLIC))
        } else if (visibilityString == VisibilityString.VisibilityInherited) {
          return Ast(newModifierNode(ModifierTypes.PUBLIC))
        } else {
          throw new RuntimeException(s"Unknown visibility string: $visibilityString")
        }
      }
      case visibilityOther: VisibilityOther =>
        astForVisibilityRestricted(filename, parentFullname, visibilityOther.restricted)
    }
  }

  def astForVisibilityRestricted(
    filename: String,
    parentFullname: String,
    visibilityRestrictedInstance: Option[VisibilityRestricted]
  ): Ast = {
    if (!visibilityRestrictedInstance.isDefined) {
      return Ast(newModifierNode(Defines.Unknown))
    }

    return Ast(newModifierNode(ModifierTypes.PUBLIC))
  }

  def modifierForVisibility(
    filename: String,
    parentFullname: String,
    visibilityInstance: Option[Visibility]
  ): (NewModifier, String) = {
    if (!visibilityInstance.isDefined) {
      return (newModifierNode(Defines.Unknown), Defines.Unknown)
    }

    visibilityInstance.get match {
      case visibilityString: VisibilityString => {
        if (visibilityString == VisibilityString.VisibilityPublic) {
          (newModifierNode(ModifierTypes.PUBLIC), ModifierTypes.PUBLIC)
        } else if (visibilityString == VisibilityString.VisibilityInherited) {
          (newModifierNode(ModifierTypes.PUBLIC), ModifierTypes.PUBLIC)
        } else {
          throw new RuntimeException(s"Unknown visibility string: $visibilityString")
        }
      }
      case visibilityOther: VisibilityOther =>
        modifierForVisibilityRestricted(filename, parentFullname, visibilityOther.restricted)
    }
  }

  def modifierForVisibilityRestricted(
    filename: String,
    parentFullname: String,
    VisibilityRestrictedInstance: Option[VisibilityRestricted]
  ): (NewModifier, String) = {
    if (!VisibilityRestrictedInstance.isDefined) {
      return (newModifierNode(Defines.Unknown), Defines.Unknown)
    }

    return (newModifierNode(ModifierTypes.PUBLIC), ModifierTypes.PUBLIC)
  }
}
