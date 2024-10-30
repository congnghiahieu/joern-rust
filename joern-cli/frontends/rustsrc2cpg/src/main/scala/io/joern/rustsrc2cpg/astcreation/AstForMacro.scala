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

trait AstForMacro(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForMacro(filename: String, parentFullname: String, macroInstance: Macro): Ast = {
    val (methodFullName, input, code) = codeForMacro(filename, parentFullname, macroInstance)

    val argAst = macroInstance.tokens match {
      case Some(tokens) => astForTokenStream(filename, parentFullname, tokens)
      case None         => Ast()
    }

    val callExprNode =
      callNode(macroInstance, code, methodFullName, methodFullName, DispatchTypes.STATIC_DISPATCH, None, None)
    callAst(callExprNode, Seq(argAst))
  }

  def codeForMacro(filename: String, parentFullname: String, macroInstance: Macro): (String, String, String) = {
    val methodFullName = macroInstance.path match {
      case Some(path) => s"${typeFullnameForPath(filename, parentFullname, path)}!"
      case None       => Defines.Unknown
    }
    val input = macroInstance.tokens match {
      case Some(tokens) => codeForTokenStream(filename, parentFullname, tokens)
      case None         => Defines.Unknown
    }
    val code = macroInstance.delimiter match {
      case Some(delimiter) => {
        delimiter match {
          case MacroDelimiter.Brace   => s"$methodFullName{$input}"
          case MacroDelimiter.Paren   => s"$methodFullName($input)"
          case MacroDelimiter.Bracket => s"$methodFullName[$input]"
        }
      }
      case None => {
        s"$methodFullName $input"
      }
    }

    (methodFullName, input, code)
  }
}
