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

trait AstForUseTree(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForUseTree(filename: String, parentFullname: String, useTreeInstance: UseTree): Ast = {
    useTreeInstance match {
      case glob: UseTreeGlob => {
        val node = NewImport()
        Ast(node)
      }
      case notGlob: UseTreeNotGlob =>
        if (notGlob.pathUseTree.isDefined) {
          astForUsePath(filename, parentFullname, notGlob.pathUseTree.get)
        } else if (notGlob.ident.isDefined) {
          astForUseName(filename, parentFullname, notGlob.ident.get)
        } else if (notGlob.renameUseTree.isDefined) {
          astForUseRename(filename, parentFullname, notGlob.renameUseTree.get)
        } else if (notGlob.groupUseTree.isDefined) {
          astForUseGroup(filename, parentFullname, notGlob.groupUseTree.get)
        } else {
          throw new RuntimeException("Unknown UseTreeNotGlob type")
        }
    }
  }

  private def astForUsePath(filename: String, parentFullname: String, usePathInstance: UsePath): Ast = {
    val node = NewImport()
    Ast(node)
  }

  private def astForUseName(filename: String, parentFullname: String, ident: Ident): Ast = {
    val node = NewImport()
      .importedEntity(ident)
      .importedAs(ident)
    Ast(node)
  }

  private def astForUseRename(filename: String, parentFullname: String, useRenameInstance: UseRename): Ast = {
    val node = NewImport()
    Ast(node)
  }

  private def astForUseGroup(filename: String, parentFullname: String, useGroupInstance: UseGroup): Ast = {
    val node = NewImport()
    Ast(node)
  }
}
