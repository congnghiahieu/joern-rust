package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
import io.joern.rustsrc2cpg.ast.Literal
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.utils.NodeBuilders.newThisParameterNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.collection.mutable.ListBuffer

trait AstForTokenTree(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForTokenStream(filename: String, parentFullname: String, tokenStream: TokenStream): Ast = {
    val code          = codeForTokenStream(filename, parentFullname, tokenStream)
    val unknownNode   = NewLiteral().code(code)
    val tokenTreeAsts = tokenStream.map(astForTokenTree(filename, parentFullname, _)).toList

    Ast(unknownNode).withChildren(tokenTreeAsts)
  }

  def astForTokenTree(filename: String, parentFullname: String, tokenTreeInstance: TokenTree): Ast = {
    if (tokenTreeInstance.group.isDefined) {
      astForGroup(filename, parentFullname, tokenTreeInstance.group.get)
    } else if (tokenTreeInstance.ident.nonEmpty) {
      astForIdent(filename, parentFullname, tokenTreeInstance.ident.get)
    } else if (tokenTreeInstance.punct.isDefined) {
      astForPunct(filename, parentFullname, tokenTreeInstance.punct.get)
    } else if (tokenTreeInstance.lit.nonEmpty) {
      astForLiteral(filename, parentFullname, tokenTreeInstance.lit)
    } else {
      throw new RuntimeException(s"Unknown tokenTree type: $tokenTreeInstance")
    }
  }

  private def astForGroup(filename: String, parentFullname: String, groupInstance: Group): Ast = {
    val asts = ListBuffer[Ast]()

    groupInstance.stream.foreach { stream =>
      stream.foreach { tokenTree =>
        asts += astForTokenTree(filename, parentFullname, tokenTree)
      }
    }

    val node = NewLiteral()

    Ast(node).withChildren(asts.toList)
  }

  private def astForIdent(filename: String, parentFullname: String, identInstance: Ident): Ast = {
    val node = NewIdentifier().name(identInstance)
    Ast(node)
  }

  private def astForPunct(filename: String, parentFullname: String, punctInstance: Punct): Ast = {
    val node = NewLiteral().code(punctInstance.op.getOrElse(""))
    Ast(node)
  }

  private def astForLiteral(filename: String, parentFullname: String, literalInstance: Literal): Ast = {
    val node = NewLiteral().code(literalInstance)
    Ast(node)
  }
}

trait CodeForTokenTree(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def codeForTokenStream(filename: String, parentFullname: String, tokenStream: TokenStream): String = {
    tokenStream.map(codeForTokenTree(filename, parentFullname, _)).mkString("")
  }

  def codeForTokenTree(filename: String, parentFullname: String, tokenTreeInstance: TokenTree): String = {
    if (tokenTreeInstance.group.isDefined) {
      codeForGroup(filename, parentFullname, tokenTreeInstance.group.get)
    } else if (tokenTreeInstance.ident.nonEmpty) {
      codeForIdent(filename, parentFullname, tokenTreeInstance.ident.get)
    } else if (tokenTreeInstance.punct.isDefined) {
      codeForPunct(filename, parentFullname, tokenTreeInstance.punct.get)
    } else if (tokenTreeInstance.lit.nonEmpty) {
      codeForLiteral(filename, parentFullname, tokenTreeInstance.lit)
    } else {
      throw new RuntimeException(s"Unknown tokenTree type: $tokenTreeInstance")
    }
  }

  private def codeForGroup(filename: String, parentFullname: String, groupInstance: Group): String = {
    val streamAsString = groupInstance.stream match {
      case Some(stream) => codeForTokenStream(filename, parentFullname, stream)
      case None         => ""
    }
    if (!groupInstance.delimiter.isDefined) {
      return streamAsString
    }

    groupInstance.delimiter.get match {
      case Delimiter.Parenthesis => s"(${streamAsString})"
      case Delimiter.Brace       => s"{${streamAsString}}"
      case Delimiter.Bracket     => s"[${streamAsString}]"
      case Delimiter.None        => s""
    }
  }

  private def codeForDelimiter(filename: String, parentFullname: String, delimiterInstance: Delimiter): String = {
    ""
  }

  private def codeForIdent(filename: String, parentFullname: String, identInstance: Ident): String = {
    identInstance
  }

  private def codeForPunct(filename: String, parentFullname: String, punctInstance: Punct): String = {
    punctInstance.op.getOrElse("")
  }

  private def codeForLiteral(filename: String, parentFullname: String, literalInstance: Literal): String = {
    literalInstance
  }
}
