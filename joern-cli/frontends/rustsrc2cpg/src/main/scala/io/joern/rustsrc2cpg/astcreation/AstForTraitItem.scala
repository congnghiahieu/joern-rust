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

trait AstForTraitItem(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForTraitItem(filename: String, parentFullname: String, traitItemInstance: TraitItem): Ast = {
    if (traitItemInstance.constTraitItem.isDefined) {
      return astForTraitItemConst(filename, parentFullname, traitItemInstance.constTraitItem.get)
    } else if (traitItemInstance.fnTraitItem.isDefined) {
      return astForTraitItemFn(filename, parentFullname, traitItemInstance.fnTraitItem.get)
    } else if (traitItemInstance.typeTraitItem.isDefined) {
      return astForTraitItemType(filename, parentFullname, traitItemInstance.typeTraitItem.get)
    } else if (traitItemInstance.macroTraitItem.isDefined) {
      return astForTraitItemMacro(filename, parentFullname, traitItemInstance.macroTraitItem.get)
    } else if (traitItemInstance.verbatimTraitItem.isDefined) {
      return astForTokenStream(filename, parentFullname, traitItemInstance.verbatimTraitItem.get)
    } else {
      throw new RuntimeException("Unknown trait item type")
    }
  }

  def astForTraitItemConst(filename: String, parentFullname: String, traitItemConst: TraitItemConst): Ast = {
    val annotationsAst = traitItemConst.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val typeAst = traitItemConst.ty match {
      case Some(ty) => astForType(filename, parentFullname, ty)
      case None     => Ast()
    }
    val genericAst = traitItemConst.generics match {
      case Some(generics) => astForGenerics(filename, parentFullname, generics)
      case None           => Ast()
    }
    val defaultAst = traitItemConst.default match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }

    val typeFullName = traitItemConst.ty match {
      case Some(ty) => typeFullnameForType(filename, parentFullname, ty)
      case None     => Defines.Unknown
    }
    val defaultCode = traitItemConst.default match {
      case Some(expr) => codeForExpr(filename, parentFullname, expr)
      case None       => Defines.Unknown
    }
    val localCode = s"const ${traitItemConst.ident}: ${typeFullName}"
    val newLocal  = localNode(traitItemConst, traitItemConst.ident, localCode, typeFullName)

    val fullCode = s"${localCode} = ${defaultCode}"
    Ast(memberNode(traitItemConst, traitItemConst.ident, fullCode, typeFullName))
      .withChild(Ast(newLocal))
      .withChild(typeAst)
      .withChild(defaultAst)
      .withChild(genericAst)
      .withChildren(annotationsAst)
  }

  def astForTraitItemFn(filename: String, parentFullname: String, traitItemFn: TraitItemFn): Ast = {
    val annotationsAst = traitItemFn.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val newMethodNode = methodNode(traitItemFn, traitItemFn.ident, traitItemFn.ident, "", filename)
    val parameterIns  = traitItemFn.inputs.map(astForFnArg(filename, parentFullname, _)).toList
    val methodRetNode = traitItemFn.output match {
      case Some(output) => {
        val typeFullname = typeFullnameForType(filename, parentFullname, output)
        methodReturnNode(UnknownAst(), typeFullname).code(typeFullname)
      }
      case None => methodReturnNode(UnknownAst(), "")
    }
    val variadicAst = traitItemFn.variadic match {
      case Some(variadic) => astForVariadic(filename, parentFullname, variadic)
      case _              => Ast()
    }
    val genericsAst = traitItemFn.generics match {
      case Some(generics) => astForGenerics(filename, parentFullname, generics)
      case None           => Ast()
    }

    val methodAst = traitItemFn.default match {
      case Some(default) => {
        val blockAst = astForBlock(filename, parentFullname, default)
        methodAstWithAnnotations(
          newMethodNode,
          parameterIns :+ variadicAst,
          blockAst,
          methodRetNode,
          Nil,
          annotationsAst
        ).withChild(genericsAst)
      }
      case None =>
        methodStubAst(newMethodNode, parameterIns :+ variadicAst, methodRetNode)
          .withChild(genericsAst)
          .withChildren(annotationsAst)
    }

    Ast(memberNode(traitItemFn, "", "", ""))
      .withChild(methodAst)
  }

  def astForTraitItemType(filename: String, parentFullname: String, traitItemType: TraitItemType): Ast = {
    val annotationsAst = traitItemType.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val genericsAst = traitItemType.generics match {
      case Some(generics) => astForGenerics(filename, parentFullname, generics)
      case None           => Ast()
    }
    val boundsAst = traitItemType.bounds.map(astForTypeParamBound(filename, parentFullname, _)).toList
    val defaultAst = traitItemType.default match {
      case Some(default) => astForType(filename, parentFullname, default)
      case None          => Ast()
    }

    var code = s"type ${traitItemType.ident}"
    code = traitItemType.bounds.nonEmpty match {
      case true =>
        val boundsCode =
          traitItemType.bounds.map(bound => codeForTypeParamBound(filename, parentFullname, bound)).mkString(" + ")
        s"$code: ${boundsCode}"
      case false => code
    }
    code = traitItemType.default match {
      case Some(default) => s"$code = ${typeFullnameForType(filename, parentFullname, default)}"
      case None          => code
    }

    val newTypeDecl = typeDeclNode(traitItemType, traitItemType.ident, traitItemType.ident, filename, code)
    Ast(memberNode(traitItemType, "", "", ""))
      .withChild(Ast(newTypeDecl))
      .withChild(defaultAst)
      .withChild(genericsAst)
      .withChildren(boundsAst)
      .withChildren(annotationsAst)

  }

  def astForTraitItemMacro(filename: String, parentFullname: String, traitItemMacro: TraitItemMacro): Ast = {
    val annotationsAst = traitItemMacro.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val macroInstance =
      Macro(traitItemMacro.path, traitItemMacro.delimiter, traitItemMacro.tokens)
    val macroAst = astForMacro(filename, parentFullname, macroInstance).withChildren(annotationsAst)

    Ast(memberNode(traitItemMacro, "", "", ""))
      .withChild(macroAst)
  }
}
