package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
import io.joern.rustsrc2cpg.ast.Type
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

trait AstForType(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForType(filename: String, parentFullname: String, typeInstance: Type): Ast = {
    typeInstance match {
      case typeNoValue: TypeNoValue =>
        val node = typeRefNode(typeNoValue, typeNoValue.toString, typeNoValue.toString)
        Ast(node)
      case typeHasValue: TypeHasValue =>
        astForTypeHasValue(filename, parentFullname, typeHasValue)
    }
  }

  def astForTypeHasValue(filename: String, parentFullname: String, typeHasValueInstance: TypeHasValue): Ast = {
    if (typeHasValueInstance.arrayType.isDefined) {
      astForTypeArray(filename, parentFullname, typeHasValueInstance.arrayType.get)
    } else if (typeHasValueInstance.bareFnType.isDefined) {
      astForTypeBareFn(filename, parentFullname, typeHasValueInstance.bareFnType.get)
    } else if (typeHasValueInstance.groupType.isDefined) {
      astForTypeGroup(filename, parentFullname, typeHasValueInstance.groupType.get)
    } else if (typeHasValueInstance.implTraitType.isDefined) {
      astForTypeImplTrait(filename, parentFullname, typeHasValueInstance.implTraitType.get)
    } else if (typeHasValueInstance.macroType.isDefined) {
      astForTypeMacro(filename, parentFullname, typeHasValueInstance.macroType.get)
    } else if (typeHasValueInstance.parenType.isDefined) {
      astForTypeParen(filename, parentFullname, typeHasValueInstance.parenType.get)
    } else if (typeHasValueInstance.pathType.isDefined) {
      astForTypePath(filename, parentFullname, typeHasValueInstance.pathType.get)
    } else if (typeHasValueInstance.ptrType.isDefined) {
      astForTypePtr(filename, parentFullname, typeHasValueInstance.ptrType.get)
    } else if (typeHasValueInstance.referenceType.isDefined) {
      astForTypeReference(filename, parentFullname, typeHasValueInstance.referenceType.get)
    } else if (typeHasValueInstance.sliceType.isDefined) {
      astForTypeSlice(filename, parentFullname, typeHasValueInstance.sliceType.get)
    } else if (typeHasValueInstance.traitObjectType.isDefined) {
      astForTypeTraitObject(filename, parentFullname, typeHasValueInstance.traitObjectType.get)
    } else if (typeHasValueInstance.tupleType.isDefined) {
      astForTypeTuple(filename, parentFullname, typeHasValueInstance.tupleType.get)
    } else if (typeHasValueInstance.verbatimType.isDefined) {
      astForTokenStream(filename, parentFullname, typeHasValueInstance.verbatimType.get)
    } else {
      throw new IllegalArgumentException("Unsupported type has value instance")
    }
  }

  def astForTypeArray(filename: String, parentFullname: String, typeArrayInstance: TypeArray): Ast = {
    val typeFullname = typeFullnameForTypeArray(filename, parentFullname, typeArrayInstance)
    val node         = typeRefNode(typeArrayInstance, typeFullname, typeFullname)
    Ast(node)
  }

  def astForTypeBareFn(filename: String, parentFullname: String, typeBareFnInstance: TypeBareFn): Ast = {
    val typeFullname = typeFullnameForTypeBareFn(filename, parentFullname, typeBareFnInstance)
    val node         = typeRefNode(typeBareFnInstance, typeFullname, typeFullname)
    Ast(node)
  }

  def astForTypeGroup(filename: String, parentFullname: String, typeGroupInstance: TypeGroup): Ast = {
    val typeFullname = typeFullnameForTypeGroup(filename, parentFullname, typeGroupInstance)
    val node         = typeRefNode(typeGroupInstance, typeFullname, typeFullname)
    Ast(node)
  }

  def astForTypeImplTrait(filename: String, parentFullname: String, typeImplTraitInstance: TypeImplTrait): Ast = {
    val typeFullname = typeFullnameForTypeImplTrait(filename, parentFullname, typeImplTraitInstance)
    val node         = typeRefNode(typeImplTraitInstance, typeFullname, typeFullname)
    Ast(node)
  }

  def astForTypeMacro(filename: String, parentFullname: String, typeMacroInstance: TypeMacro): Ast = {
    val typeFullname = typeFullnameForTypeMacro(filename, parentFullname, typeMacroInstance)
    val node         = typeRefNode(typeMacroInstance, typeFullname, typeFullname)
    Ast(node)
  }

  def astForTypeParen(filename: String, parentFullname: String, typeParenInstance: TypeParen): Ast = {
    val typeFullname = typeFullnameForTypeParen(filename, parentFullname, typeParenInstance)
    val node         = typeRefNode(typeParenInstance, typeFullname, typeFullname)
    Ast(node)
  }

  def astForTypePath(filename: String, parentFullname: String, typePathInstance: TypePath): Ast = {
    val typeFullname = typeFullnameForTypePath(filename, parentFullname, typePathInstance)
    val node         = typeRefNode(typePathInstance, typeFullname, typeFullname)
    Ast(node)
  }

  def astForTypePtr(filename: String, parentFullname: String, typePtrInstance: TypePtr): Ast = {
    val typeFullname = typeFullnameForTypePtr(filename, parentFullname, typePtrInstance)

    val node = typeRefNode(typePtrInstance, typeFullname, typeFullname)

    Ast(node)
  }

  def astForTypeReference(filename: String, parentFullname: String, typeReferenceInstance: TypeReference): Ast = {
    val typeFullname = typeFullnameForTypeReference(filename, parentFullname, typeReferenceInstance)
    val node         = typeRefNode(typeReferenceInstance, typeFullname, typeFullname)
    Ast(node)
  }

  def astForTypeSlice(filename: String, parentFullname: String, typeSliceInstance: TypeSlice): Ast = {
    val typeFullname = typeFullnameForTypeSlice(filename, parentFullname, typeSliceInstance)
    val node         = typeRefNode(typeSliceInstance, typeFullname, typeFullname)
    Ast(node)
  }

  def astForTypeTraitObject(filename: String, parentFullname: String, typeTraitObjectInstance: TypeTraitObject): Ast = {
    val typeFullname = typeFullnameForTypeTraitObject(filename, parentFullname, typeTraitObjectInstance)
    val node         = typeRefNode(typeTraitObjectInstance, typeFullname, typeFullname)
    Ast(node)
  }

  def astForTypeTuple(filename: String, parentFullname: String, typeTupleInstance: TypeTuple): Ast = {
    val typeFullname = typeFullnameForTypeTuple(filename, parentFullname, typeTupleInstance)
    val node         = typeRefNode(typeTupleInstance, typeFullname, typeFullname)
    Ast(node)
  }
}

trait TypeFullnameForType(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def typeFullnameForType(filename: String, parentFullname: String, typeInstance: Type): String = {
    typeInstance match {
      case typeNoValue: TypeNoValue =>
        typeNoValue.toString
      case typeHasValue: TypeHasValue =>
        typeFullnameForTypeHasValue(filename, parentFullname, typeHasValue)
    }
  }

  def typeFullnameForTypeHasValue(
    filename: String,
    parentFullname: String,
    typeHasValueInstance: TypeHasValue
  ): String = {
    if (typeHasValueInstance.arrayType.isDefined) {
      typeFullnameForTypeArray(filename, parentFullname, typeHasValueInstance.arrayType.get)
    } else if (typeHasValueInstance.bareFnType.isDefined) {
      typeFullnameForTypeBareFn(filename, parentFullname, typeHasValueInstance.bareFnType.get)
    } else if (typeHasValueInstance.groupType.isDefined) {
      typeFullnameForTypeGroup(filename, parentFullname, typeHasValueInstance.groupType.get)
    } else if (typeHasValueInstance.implTraitType.isDefined) {
      typeFullnameForTypeImplTrait(filename, parentFullname, typeHasValueInstance.implTraitType.get)
    } else if (typeHasValueInstance.macroType.isDefined) {
      typeFullnameForTypeMacro(filename, parentFullname, typeHasValueInstance.macroType.get)
    } else if (typeHasValueInstance.parenType.isDefined) {
      typeFullnameForTypeParen(filename, parentFullname, typeHasValueInstance.parenType.get)
    } else if (typeHasValueInstance.pathType.isDefined) {
      typeFullnameForTypePath(filename, parentFullname, typeHasValueInstance.pathType.get)
    } else if (typeHasValueInstance.ptrType.isDefined) {
      typeFullnameForTypePtr(filename, parentFullname, typeHasValueInstance.ptrType.get)
    } else if (typeHasValueInstance.referenceType.isDefined) {
      typeFullnameForTypeReference(filename, parentFullname, typeHasValueInstance.referenceType.get)
    } else if (typeHasValueInstance.sliceType.isDefined) {
      typeFullnameForTypeSlice(filename, parentFullname, typeHasValueInstance.sliceType.get)
    } else if (typeHasValueInstance.traitObjectType.isDefined) {
      typeFullnameForTypeTraitObject(filename, parentFullname, typeHasValueInstance.traitObjectType.get)
    } else if (typeHasValueInstance.tupleType.isDefined) {
      typeFullnameForTypeTuple(filename, parentFullname, typeHasValueInstance.tupleType.get)
    } else if (typeHasValueInstance.verbatimType.isDefined) {
      codeForTokenStream(filename, parentFullname, typeHasValueInstance.verbatimType.get)
    } else {
      throw new IllegalArgumentException("Unsupported type has value instance")
    }
  }

  def typeFullnameForTypeArray(filename: String, parentFullname: String, typeArrayInstance: TypeArray): String = {
    if (!typeArrayInstance.elem.isDefined) {
      return ""
    }

    val elemenTypeFullname = typeFullnameForType(filename, parentFullname, typeArrayInstance.elem.get)

    typeArrayInstance.len match {
      case Some(len) => {
        val lenCode = codeForExpr(filename, parentFullname, len)
        s"[$elemenTypeFullname; $lenCode]"
      }
      case None => s"[$elemenTypeFullname]"
    }
  }

  def typeFullnameForTypeBareFn(filename: String, parentFullname: String, typeBareFnInstance: TypeBareFn): String = {
    // Lack
    ""
  }

  def typeFullnameForTypeGroup(filename: String, parentFullname: String, typeGroupInstance: TypeGroup): String = {
    // Lack
    ""
  }

  def typeFullnameForTypeImplTrait(
    filename: String,
    parentFullname: String,
    typeImplTraitInstance: TypeImplTrait
  ): String = {
    val boundsCode =
      typeImplTraitInstance.bounds.map(codeForTypeParamBound(filename, parentFullname, _)).mkString(" + ")
    boundsCode
  }

  def typeFullnameForTypeMacro(filename: String, parentFullname: String, typeMacroInstance: TypeMacro): String = {
    // Lack
    ""
  }

  def typeFullnameForTypeParen(filename: String, parentFullname: String, typeParenInstance: TypeParen): String = {
    if (!typeParenInstance.elem.isDefined) {
      return ""
    }

    val elemenTypeFullname = typeFullnameForType(filename, parentFullname, typeParenInstance.elem.get)

    return s"($elemenTypeFullname)"
  }

  def typeFullnameForTypePath(filename: String, parentFullname: String, typePathInstance: TypePath): String = {
    val (typeFullname, _, code) =
      codeForPath(filename, parentFullname, Path(typePathInstance.segments, typePathInstance.leading_colon))
    typeFullname
  }

  def typeFullnameForTypePtr(filename: String, parentFullname: String, typePtrInstance: TypePtr): String = {
    if (!typePtrInstance.elem.isDefined) {
      return ""
    }

    val elemenTypeFullname = typeFullnameForType(filename, parentFullname, typePtrInstance.elem.get)

    if (typePtrInstance.mut.getOrElse(false)) {
      return s"*mut $elemenTypeFullname"
    }

    if (typePtrInstance.const.getOrElse(false)) {
      return s"*const $elemenTypeFullname"
    }

    ""
  }

  def typeFullnameForTypeReference(
    filename: String,
    parentFullname: String,
    typeReferenceInstance: TypeReference
  ): String = {
    var code = typeReferenceInstance.lifetime match {
      case Some(lifetime) => {
        val lifetimeCode = codeForLifetime(filename, parentFullname, lifetime)
        s"&$lifetimeCode"
      }
      case None => "&"
    }
    code = typeReferenceInstance.mut match {
      case Some(true) => s"${code} mut"
      case _          => code
    }
    code = typeReferenceInstance.elem match {
      case Some(elem) => {
        val typeFullname = typeFullnameForType(filename, parentFullname, elem)
        s"${code} $typeFullname"
      }
      case None => code
    }

    code
  }

  def typeFullnameForTypeSlice(filename: String, parentFullname: String, typeSliceInstance: TypeSlice): String = {
    if (!typeSliceInstance.elem.isDefined) {
      return ""
    }

    val elemenTypeFullname = typeFullnameForType(filename, parentFullname, typeSliceInstance.elem.get)

    s"[$elemenTypeFullname]"
  }

  def typeFullnameForTypeTraitObject(
    filename: String,
    parentFullname: String,
    typeTraitObjectInstance: TypeTraitObject
  ): String = {
    val boundsCode =
      typeTraitObjectInstance.bounds.map(codeForTypeParamBound(filename, parentFullname, _)).mkString(" + ")
    typeTraitObjectInstance.dyn match {
      case Some(true) => s"dyn $boundsCode"
      case _          => s"${boundsCode}"
    }
  }

  def typeFullnameForTypeTuple(filename: String, parentFullname: String, typeTupleInstance: TypeTuple): String = {
    val tupleTypes = typeTupleInstance.elems.map(typeFullnameForType(filename, parentFullname, _))
    s"(${tupleTypes.mkString(", ")})"
  }
}
