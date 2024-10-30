package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.collection.mutable.ListBuffer

trait AstForLit(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForLit(filename: String, parentFullname: String, litExprInstance: Lit): Ast = {
    val value        = codeForLit(filename, parentFullname, litExprInstance)
    val typeFullname = typeFullnameForLit(filename, parentFullname, litExprInstance)
    val node         = literalNode(litExprInstance, value, typeFullname)
    Ast(node)
  }

  def codeForLit(filename: String, parentFullname: String, litExprInstance: Lit): String = litExprInstance match {
    case _ if litExprInstance.strLit.isDefined      => litExprInstance.strLit.get
    case _ if litExprInstance.byteStrLit.isDefined  => litExprInstance.byteStrLit.get
    case _ if litExprInstance.byteLit.isDefined     => litExprInstance.byteLit.get
    case _ if litExprInstance.charLit.isDefined     => litExprInstance.charLit.get
    case _ if litExprInstance.intLit.isDefined      => litExprInstance.intLit.get
    case _ if litExprInstance.floatLit.isDefined    => litExprInstance.floatLit.get
    case _ if litExprInstance.boolLit.isDefined     => litExprInstance.boolLit.get
    case _ if litExprInstance.verbatimLit.isDefined => litExprInstance.verbatimLit.get
    case _                                          => throw new RuntimeException("Unsupported literal type")
  }

  def typeFullnameForLit(filename: String, parentFullname: String, litExprInstance: Lit): String =
    litExprInstance match {
      case _ if litExprInstance.strLit.isDefined      => Primitives.STR
      case _ if litExprInstance.byteStrLit.isDefined  => Primitives.U8
      case _ if litExprInstance.byteLit.isDefined     => Primitives.U8
      case _ if litExprInstance.charLit.isDefined     => Primitives.CHAR
      case _ if litExprInstance.intLit.isDefined      => Primitives.ISIZE
      case _ if litExprInstance.floatLit.isDefined    => Primitives.F32
      case _ if litExprInstance.boolLit.isDefined     => Primitives.BOOL
      case _ if litExprInstance.verbatimLit.isDefined => Primitives.STR
      case _                                          => throw new RuntimeException("Unsupported literal type")
    }
}
