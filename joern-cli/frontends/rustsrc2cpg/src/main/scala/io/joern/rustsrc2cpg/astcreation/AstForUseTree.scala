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
        val node = newImportNode(glob.toString, glob.toString, glob.toString, glob).isWildcard(true)
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
    val node = newImportNode("", "", "", usePathInstance).isExplicit(true)
    Ast(node)
  }

  private def astForUseName(filename: String, parentFullname: String, ident: Ident): Ast = {
    val node = newImportNode("", ident, ident, UnknownAst()).isExplicit(true)
    Ast(node)
  }

  private def astForUseRename(filename: String, parentFullname: String, useRenameInstance: UseRename): Ast = {
    val node = newImportNode("", useRenameInstance.ident, useRenameInstance.rename, useRenameInstance)
      .isExplicit(true)
      .explicitAs(true)
    Ast(node)
  }

  private def astForUseGroup(filename: String, parentFullname: String, useGroupInstance: UseGroup): Ast = {
    val node = newImportNode("", "", "", UnknownAst()).isExplicit(true)
    Ast(node)
  }
}

trait CodeForUseTree(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def codeForUseTree(filename: String, parentFullname: String, useTreeInstance: UseTree): String = {
    useTreeInstance match {
      case glob: UseTreeGlob => glob.toString
      case notGlob: UseTreeNotGlob =>
        if (notGlob.pathUseTree.isDefined) {
          codeForUsePath(filename, parentFullname, notGlob.pathUseTree.get)
        } else if (notGlob.ident.isDefined) {
          codeForUseName(filename, parentFullname, notGlob.ident.get)
        } else if (notGlob.renameUseTree.isDefined) {
          codeForUseRename(filename, parentFullname, notGlob.renameUseTree.get)
        } else if (notGlob.groupUseTree.isDefined) {
          codeForUseGroup(filename, parentFullname, notGlob.groupUseTree.get)
        } else {
          throw new RuntimeException("Unknown UseTreeNotGlob type")
        }
    }
  }

  private def codeForUsePath(filename: String, parentFullname: String, usePathInstance: UsePath): String = {
    usePathInstance.tree match {
      case Some(tree) => s"${usePathInstance.ident}::${codeForUseTree(filename, parentFullname, tree)}"
      case None       => usePathInstance.ident
    }
  }

  private def codeForUseName(filename: String, parentFullname: String, ident: Ident): String = {
    ident.toString
  }

  private def codeForUseRename(filename: String, parentFullname: String, useRenameInstance: UseRename): String = {
    s"${useRenameInstance.ident} as ${useRenameInstance.rename}"
  }

  private def codeForUseGroup(filename: String, parentFullname: String, useGroupInstance: UseGroup): String = {
    s"{${useGroupInstance.map(codeForUseTree(filename, parentFullname, _)).mkString(", ")}}"
  }
}
