package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.collection.mutable.ListBuffer

trait AstForPat(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForPat(filename: String, parentFullname: String, patInstance: Pat): Ast = {
    if (patInstance.constPat.isDefined) {
      astForExprConst(filename, parentFullname, patInstance.constPat.get)
    } else if (patInstance.identPat.isDefined) {
      astForPatIdent(filename, parentFullname, patInstance.identPat.get)
    } else if (patInstance.litPat.isDefined) {
      astForExprLit(filename, parentFullname, patInstance.litPat.get)
    } else if (patInstance.macroPat.isDefined) {
      astForExprMacro(filename, parentFullname, patInstance.macroPat.get)
    } else if (patInstance.orPat.isDefined) {
      astForPatOr(filename, parentFullname, patInstance.orPat.get)
    } else if (patInstance.parenPat.isDefined) {
      astForPatParen(filename, parentFullname, patInstance.parenPat.get)
    } else if (patInstance.pathPat.isDefined) {
      astForExprPath(filename, parentFullname, patInstance.pathPat.get)
    } else if (patInstance.rangePat.isDefined) {
      astForExprRange(filename, parentFullname, patInstance.rangePat.get)
    } else if (patInstance.referencePat.isDefined) {
      astForPatReference(filename, parentFullname, patInstance.referencePat.get)
    } else if (patInstance.restPat.isDefined) {
      astForPatRest(filename, parentFullname, patInstance.restPat.get)
    } else if (patInstance.slicePat.isDefined) {
      astForPatSlice(filename, parentFullname, patInstance.slicePat.get)
    } else if (patInstance.structPat.isDefined) {
      astForPatStruct(filename, parentFullname, patInstance.structPat.get)
    } else if (patInstance.tuplePat.isDefined) {
      astForPatTuple(filename, parentFullname, patInstance.tuplePat.get)
    } else if (patInstance.tupleStructPat.isDefined) {
      astForPatTupleStruct(filename, parentFullname, patInstance.tupleStructPat.get)
    } else if (patInstance.typePat.isDefined) {
      astForPatType(filename, parentFullname, patInstance.typePat.get)
    } else if (patInstance.verbatimPat.isDefined) {
      astForTokenStream(filename, parentFullname, patInstance.verbatimPat.get)
    } else if (patInstance.wildPat.isDefined) {
      astForPatWild(filename, parentFullname, patInstance.wildPat.get)
    } else {
      throw new IllegalArgumentException("Unsupported pattern type")
    }
  }

  def astForPatIdent(filename: String, parentFullname: String, identPatInstance: PatIdent): Ast = {
    val annotationsAst = identPatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val subpatAst = identPatInstance.subpat match {
      case Some(subpat) => astForPat(filename, parentFullname, subpat)
      case None         => Ast()
    }

    val code = codeForPatIdent(filename, parentFullname, identPatInstance)
    val identNode =
      identifierNode(identPatInstance, identPatInstance.ident, code, "")

    Ast(unknownNode(identPatInstance, code))
      .withChild(Ast(identNode))
      .withChild(subpatAst)
      .withChildren(annotationsAst)
  }

  def astForPatOr(filename: String, parentFullname: String, orPatInstance: PatOr): Ast = {
    val annotationsAst = orPatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val casesAst = orPatInstance.cases.map(astForPat(filename, parentFullname, _)).toList
    val code     = codeForPatOr(filename, parentFullname, orPatInstance)

    Ast(unknownNode(orPatInstance, ""))
      .withChildren(casesAst)
      .withChildren(annotationsAst)
  }

  def astForPatParen(filename: String, parentFullname: String, parenPatInstance: PatParen): Ast = {
    val annotationsAst = parenPatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val patAst = parenPatInstance.pat match {
      case Some(pat) => astForPat(filename, parentFullname, pat)
      case None      => Ast()
    }

    Ast(unknownNode(parenPatInstance, ""))
      .withChild(patAst)
      .withChildren(annotationsAst)
  }

  def astForPatReference(filename: String, parentFullname: String, referencePatInstance: PatReference): Ast = {
    val annotationsAst = referencePatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val patAst = referencePatInstance.pat match {
      case Some(pat) => astForPat(filename, parentFullname, pat)
      case None      => Ast()
    }
    val code = codeForPatReference(filename, parentFullname, referencePatInstance)

    Ast(unknownNode(referencePatInstance, "").code(code))
      .withChild(patAst)
      .withChildren(annotationsAst)
  }

  def astForPatRest(filename: String, parentFullname: String, restPatInstance: PatRest): Ast = {
    val annotationsAst = restPatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val code = codeForPatRest(filename, parentFullname, restPatInstance)
    val identNode =
      identifierNode(restPatInstance, code, code, code)
    Ast(identNode)
  }

  def astForPatSlice(filename: String, parentFullname: String, slicePatInstance: PatSlice): Ast = {
    val annotationsAst = slicePatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val elemsAst = slicePatInstance.elems.map(astForPat(filename, parentFullname, _)).toList
    val code     = codeForPatSlice(filename, parentFullname, slicePatInstance)

    Ast(unknownNode(slicePatInstance, code))
      .withChildren(elemsAst)
      .withChildren(annotationsAst)
  }

  def astForPatStruct(filename: String, parentFullname: String, structPatInstance: PatStruct): Ast = {
    val annotationsAst = structPatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val pathAst = structPatInstance.path match {
      case Some(path) => astForPath(filename, parentFullname, path)
      case None       => Ast()
    }
    val qselfAst = structPatInstance.qself match {
      case Some(qself) => astForQself(filename, parentFullname, qself)
      case None        => Ast()
    }

    val fieldsAst = structPatInstance.fields.map(astForFieldPat(filename, parentFullname, _)).toList
    val restAst = structPatInstance.rest match {
      case Some(rest) => astForPatRest(filename, parentFullname, rest)
      case None       => Ast()
    }

    Ast(unknownNode(structPatInstance, ""))
      .withChild(pathAst)
      .withChild(qselfAst)
      .withChildren(fieldsAst)
      .withChild(restAst)
      .withChildren(annotationsAst)
  }

  def astForPatTuple(filename: String, parentFullname: String, tuplePatInstance: PatTuple): Ast = {
    val annotationsAst = tuplePatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val elemsAst = tuplePatInstance.elems.map(astForPat(filename, parentFullname, _)).toList
    val code     = codeForPatTuple(filename, parentFullname, tuplePatInstance)

    Ast(unknownNode(tuplePatInstance, code))
      .withChildren(elemsAst)
      .withChildren(annotationsAst)
  }

  def astForPatTupleStruct(filename: String, parentFullname: String, tupleStructPatInstance: PatTupleStruct): Ast = {
    val annotationsAst = tupleStructPatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val pathAst = tupleStructPatInstance.path match {
      case Some(path) => astForPath(filename, parentFullname, path)
      case None       => Ast()
    }

    val elemsAst = tupleStructPatInstance.elems.map(astForPat(filename, parentFullname, _)).toList

    val code = codeForPatTupleStruct(filename, parentFullname, tupleStructPatInstance)

    Ast(unknownNode(tupleStructPatInstance, code))
      .withChild(pathAst)
      .withChildren(elemsAst)
      .withChildren(annotationsAst)
  }

  def astForPatType(filename: String, parentFullname: String, typePatInstance: PatType): Ast = {
    val annotationsAst = typePatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val patAst = typePatInstance.pat match {
      case Some(pat) => astForPat(filename, parentFullname, pat)
      case None      => Ast()
    }
    val typeAst = typePatInstance.ty match {
      case Some(ty) => astForType(filename, parentFullname, ty)
      case None     => Ast()
    }
    val code = codeForPatType(filename, parentFullname, typePatInstance)

    Ast(unknownNode(typePatInstance, code))
      .withChild(patAst)
      .withChild(typeAst)
      .withChildren(annotationsAst)
  }

  def astForPatWild(filename: String, parentFullname: String, wildPatInstance: PatWild): Ast = {
    val annotationsAst = wildPatInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val name = "_"
    val identNode =
      identifierNode(wildPatInstance, name, name, name)
    Ast(identNode)
    // .withChildren(annotationsAst)
  }
}

trait CodeForPat(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def codeForPat(filename: String, parentFullname: String, patInstance: Pat): String = {
    if (patInstance.constPat.isDefined) {
      codeForExprConst(filename, parentFullname, patInstance.constPat.get)
    } else if (patInstance.identPat.isDefined) {
      codeForPatIdent(filename, parentFullname, patInstance.identPat.get)
    } else if (patInstance.litPat.isDefined) {
      codeForExprLit(filename, parentFullname, patInstance.litPat.get)
    } else if (patInstance.macroPat.isDefined) {
      codeForExprMacro(filename, parentFullname, patInstance.macroPat.get)
    } else if (patInstance.orPat.isDefined) {
      codeForPatOr(filename, parentFullname, patInstance.orPat.get)
    } else if (patInstance.parenPat.isDefined) {
      codeForPatParen(filename, parentFullname, patInstance.parenPat.get)
    } else if (patInstance.pathPat.isDefined) {
      codeForExprPath(filename, parentFullname, patInstance.pathPat.get)
    } else if (patInstance.rangePat.isDefined) {
      codeForExprRange(filename, parentFullname, patInstance.rangePat.get)
    } else if (patInstance.referencePat.isDefined) {
      codeForPatReference(filename, parentFullname, patInstance.referencePat.get)
    } else if (patInstance.restPat.isDefined) {
      codeForPatRest(filename, parentFullname, patInstance.restPat.get)
    } else if (patInstance.slicePat.isDefined) {
      codeForPatSlice(filename, parentFullname, patInstance.slicePat.get)
    } else if (patInstance.structPat.isDefined) {
      codeForPatStruct(filename, parentFullname, patInstance.structPat.get)
    } else if (patInstance.tuplePat.isDefined) {
      codeForPatTuple(filename, parentFullname, patInstance.tuplePat.get)
    } else if (patInstance.tupleStructPat.isDefined) {
      codeForPatTupleStruct(filename, parentFullname, patInstance.tupleStructPat.get)
    } else if (patInstance.typePat.isDefined) {
      codeForPatType(filename, parentFullname, patInstance.typePat.get)
    } else if (patInstance.verbatimPat.isDefined) {
      codeForTokenStream(filename, parentFullname, patInstance.verbatimPat.get)
    } else if (patInstance.wildPat.isDefined) {
      codeForPatWild(filename, parentFullname, patInstance.wildPat.get)
    } else {
      throw new IllegalArgumentException("Unsupported pattern type")
    }
  }

  def codeForPatIdent(filename: String, parentFullname: String, identPatInstance: PatIdent): String = {
    var code = identPatInstance.ident

    if (identPatInstance.mut.isDefined) {
      code = s"mut $code"
    }
    if (identPatInstance.ref.isDefined) {
      code = s"ref $code"
    }
    identPatInstance.subpat match {
      case Some(subpat) => {
        val subPatCode = codeForPat(filename, parentFullname, subpat)
        s"$code @ $subPatCode"
      }
      case None => code
    }
  }

  def codeForPatOr(filename: String, parentFullname: String, orPatInstance: PatOr): String = {
    orPatInstance.cases.map(codeForPat(filename, parentFullname, _)).mkString(" | ")
  }
  def codeForPatParen(filename: String, parentFullname: String, parenPatInstance: PatParen): String = {
    "Paren Pattern"
  }

  def codeForPatReference(filename: String, parentFullname: String, referencePatInstance: PatReference): String = {
    referencePatInstance.mut match {
      case Some(true) => s"&mut ${codeForPat(filename, parentFullname, referencePatInstance.pat.get)}"
      case _          => s"&${codeForPat(filename, parentFullname, referencePatInstance.pat.get)}"
    }
  }
  def codeForPatRest(filename: String, parentFullname: String, restPatInstance: PatRest): String = {
    ".."
  }
  def codeForPatSlice(filename: String, parentFullname: String, slicePatInstance: PatSlice): String = {
    s"[${slicePatInstance.elems.map(codeForPat(filename, parentFullname, _)).mkString(", ")}]"
  }
  def codeForPatStruct(filename: String, parentFullname: String, structPatInstance: PatStruct): String = {
    val typeFullname = structPatInstance.path match {
      case Some(path) => typeFullnameForPath(filename, parentFullname, path)
      case None       => Defines.Unknown
    }
    val fieldsCode = s"${structPatInstance.fields.map(codeForFieldPat(filename, parentFullname, _)).mkString(", ")}"

    s"$typeFullname { $fieldsCode }"
  }
  def codeForPatTuple(filename: String, parentFullname: String, tuplePatInstance: PatTuple): String = {
    s"(${tuplePatInstance.elems.map(codeForPat(filename, parentFullname, _)).mkString(", ")})"
  }
  def codeForPatTupleStruct(
    filename: String,
    parentFullname: String,
    tupleStructPatInstance: PatTupleStruct
  ): String = {
    val typeFullname = tupleStructPatInstance.path match {
      case Some(path) => typeFullnameForPath(filename, parentFullname, path)
      case None       => Defines.Unknown
    }
    val elemsCode = tupleStructPatInstance.elems.map(codeForPat(filename, parentFullname, _)).mkString(", ")
    s"$typeFullname($elemsCode)"
  }
  def codeForPatType(filename: String, parentFullname: String, typePatInstance: PatType): String = {
    val patCode = typePatInstance.pat match {
      case Some(pat) => codeForPat(filename, parentFullname, pat)
      case None      => Defines.Unknown
    }
    val typeCode = typePatInstance.ty match {
      case Some(ty) => typeFullnameForType(filename, parentFullname, ty)
      case None     => Defines.Unknown
    }
    s"$patCode: $typeCode"
  }
  def codeForPatWild(filename: String, parentFullname: String, wildPatInstance: PatWild): String = {
    "_"
  }
}
