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

trait AstForPat(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForPat(filename: String, parentFullname: String, patInstance: Pat): Ast = {
    if (patInstance.constPat.isDefined) {
      astForPatConst(filename, parentFullname, patInstance.constPat.get)
    } else if (patInstance.identPat.isDefined) {
      astForPatIdent(filename, parentFullname, patInstance.identPat.get)
    } else if (patInstance.litPat.isDefined) {
      astForPatLit(filename, parentFullname, patInstance.litPat.get)
    } else if (patInstance.macroPat.isDefined) {
      astForPatMacro(filename, parentFullname, patInstance.macroPat.get)
    } else if (patInstance.orPat.isDefined) {
      astForPatOr(filename, parentFullname, patInstance.orPat.get)
    } else if (patInstance.parenPat.isDefined) {
      astForPatParen(filename, parentFullname, patInstance.parenPat.get)
    } else if (patInstance.pathPat.isDefined) {
      astForPatPath(filename, parentFullname, patInstance.pathPat.get)
    } else if (patInstance.rangePat.isDefined) {
      astForPatRange(filename, parentFullname, patInstance.rangePat.get)
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

  def astForPatConst(filename: String, parentFullname: String, constPatInstance: ExprConst): Ast = {
    astForExprConst(filename, parentFullname, constPatInstance)
  }
  def astForPatIdent(filename: String, parentFullname: String, identPatInstance: PatIdent): Ast = {
    Ast()
  }
  def astForPatLit(filename: String, parentFullname: String, litPatInstance: ExprLit): Ast = {
    astForExprLit(filename, parentFullname, litPatInstance)
  }
  def astForPatMacro(filename: String, parentFullname: String, macroPatInstance: ExprMacro): Ast = {
    Ast()
  }
  def astForPatOr(filename: String, parentFullname: String, orPatInstance: PatOr): Ast = {
    Ast()
  }
  def astForPatParen(filename: String, parentFullname: String, parenPatInstance: PatParen): Ast = {
    Ast()
  }
  def astForPatPath(filename: String, parentFullname: String, pathPatInstance: ExprPath): Ast = {
    Ast()
  }
  def astForPatRange(filename: String, parentFullname: String, rangePatInstance: ExprRange): Ast = {
    Ast()
  }
  def astForPatReference(filename: String, parentFullname: String, referencePatInstance: PatReference): Ast = {
    Ast()
  }
  def astForPatRest(filename: String, parentFullname: String, restPatInstance: PatRest): Ast = {
    Ast()
  }
  def astForPatSlice(filename: String, parentFullname: String, slicePatInstance: PatSlice): Ast = {
    Ast()
  }
  def astForPatStruct(filename: String, parentFullname: String, structPatInstance: PatStruct): Ast = {
    Ast()
  }
  def astForPatTuple(filename: String, parentFullname: String, tuplePatInstance: PatTuple): Ast = {
    Ast()
  }
  def astForPatTupleStruct(filename: String, parentFullname: String, tupleStructPatInstance: PatTupleStruct): Ast = {
    Ast()
  }
  def astForPatType(filename: String, parentFullname: String, typePatInstance: PatType): Ast = {
    val node = NewMethodParameterIn()
    Ast(node)
  }
  def astForPatWild(filename: String, parentFullname: String, wildPatInstance: PatWild): Ast = {
    Ast()
  }
}