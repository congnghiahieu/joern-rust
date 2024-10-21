package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.collection.mutable.ListBuffer

trait AstForExpr(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForExpr(filename: String, parentFullname: String, exprInstance: Expr): Ast = {
    if (exprInstance.arrayExpr.isDefined) {
      astForExprArray(filename, parentFullname, exprInstance.arrayExpr.get)
    } else if (exprInstance.assignExpr.isDefined) {
      astForExprAssign(filename, parentFullname, exprInstance.assignExpr.get)
    } else if (exprInstance.asyncExpr.isDefined) {
      astForExprAsync(filename, parentFullname, exprInstance.asyncExpr.get)
    } else if (exprInstance.awaitExpr.isDefined) {
      astForExprAwait(filename, parentFullname, exprInstance.awaitExpr.get)
    } else if (exprInstance.binaryExpr.isDefined) {
      astForExprBinary(filename, parentFullname, exprInstance.binaryExpr.get)
    } else if (exprInstance.blockExpr.isDefined) {
      astForExprBlock(filename, parentFullname, exprInstance.blockExpr.get)
    } else if (exprInstance.breakExpr.isDefined) {
      astForExprBreak(filename, parentFullname, exprInstance.breakExpr.get)
    } else if (exprInstance.callExpr.isDefined) {
      astForExprCall(filename, parentFullname, exprInstance.callExpr.get)
    } else if (exprInstance.castExpr.isDefined) {
      astForExprCast(filename, parentFullname, exprInstance.castExpr.get)
    } else if (exprInstance.closureExpr.isDefined) {
      astForExprClosure(filename, parentFullname, exprInstance.closureExpr.get)
    } else if (exprInstance.constExpr.isDefined) {
      astForExprConst(filename, parentFullname, exprInstance.constExpr.get)
    } else if (exprInstance.continueExpr.isDefined) {
      astForExprContinue(filename, parentFullname, exprInstance.continueExpr.get)
    } else if (exprInstance.fieldExpr.isDefined) {
      astForExprField(filename, parentFullname, exprInstance.fieldExpr.get)
    } else if (exprInstance.forLoopExpr.isDefined) {
      astForExprForLoop(filename, parentFullname, exprInstance.forLoopExpr.get)
    } else if (exprInstance.groupExpr.isDefined) {
      astForExprGroup(filename, parentFullname, exprInstance.groupExpr.get)
    } else if (exprInstance.ifExpr.isDefined) {
      astForExprIf(filename, parentFullname, exprInstance.ifExpr.get)
    } else if (exprInstance.indexExpr.isDefined) {
      astForExprIndex(filename, parentFullname, exprInstance.indexExpr.get)
    } else if (exprInstance.inferExpr.isDefined) {
      astForExprInfer(filename, parentFullname, exprInstance.inferExpr.get)
    } else if (exprInstance.letExpr.isDefined) {
      astForExprLet(filename, parentFullname, exprInstance.letExpr.get)
    } else if (exprInstance.litExpr.isDefined) {
      astForExprLit(filename, parentFullname, exprInstance.litExpr.get)
    } else if (exprInstance.loopExpr.isDefined) {
      astForExprLoop(filename, parentFullname, exprInstance.loopExpr.get)
    } else if (exprInstance.macroExpr.isDefined) {
      astForExprMacro(filename, parentFullname, exprInstance.macroExpr.get)
    } else if (exprInstance.matchExpr.isDefined) {
      astForExprMatch(filename, parentFullname, exprInstance.matchExpr.get)
    } else if (exprInstance.methodCallExpr.isDefined) {
      astForExprMethodCall(filename, parentFullname, exprInstance.methodCallExpr.get)
    } else if (exprInstance.parenExpr.isDefined) {
      astForExprParen(filename, parentFullname, exprInstance.parenExpr.get)
    } else if (exprInstance.pathExpr.isDefined) {
      astForExprPath(filename, parentFullname, exprInstance.pathExpr.get)
    } else if (exprInstance.rangeExpr.isDefined) {
      astForExprRange(filename, parentFullname, exprInstance.rangeExpr.get)
    } else if (exprInstance.referenceExpr.isDefined) {
      astForExprReference(filename, parentFullname, exprInstance.referenceExpr.get)
    } else if (exprInstance.repeatExpr.isDefined) {
      astForExprRepeat(filename, parentFullname, exprInstance.repeatExpr.get)
    } else if (exprInstance.returnExpr.isDefined) {
      astForExprReturn(filename, parentFullname, exprInstance.returnExpr.get)
    } else if (exprInstance.structExpr.isDefined) {
      astForExprStruct(filename, parentFullname, exprInstance.structExpr.get)
    } else if (exprInstance.tryExpr.isDefined) {
      astForExprTry(filename, parentFullname, exprInstance.tryExpr.get)
    } else if (exprInstance.tryBlockExpr.isDefined) {
      astForExprTryBlock(filename, parentFullname, exprInstance.tryBlockExpr.get)
    } else if (exprInstance.tupleExpr.isDefined) {
      astForExprTuple(filename, parentFullname, exprInstance.tupleExpr.get)
    } else if (exprInstance.unaryExpr.isDefined) {
      astForExprUnary(filename, parentFullname, exprInstance.unaryExpr.get)
    } else if (exprInstance.unsafeExpr.isDefined) {
      astForExprUnsafe(filename, parentFullname, exprInstance.unsafeExpr.get)
    } else if (exprInstance.verbatimExpr.isDefined) {
      astForTokenStream(filename, parentFullname, exprInstance.verbatimExpr.get)
    } else if (exprInstance.whileExpr.isDefined) {
      astForExprWhile(filename, parentFullname, exprInstance.whileExpr.get)
    } else if (exprInstance.yieldExpr.isDefined) {
      astForExprYield(filename, parentFullname, exprInstance.yieldExpr.get)
    } else {
      throw new IllegalArgumentException("Unsupported expression type")
    }
  }

  def astForExprArray(filename: String, parentFullname: String, arrayExprInstance: ExprArray): Ast = {
    val annotationsAst = arrayExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toSeq
      case None        => Seq()
    }
    val elemsAst = arrayExprInstance.elems.map(astForExpr(filename, parentFullname, _)).toList
    val code     = s"[${arrayExprInstance.elems.map(codeForExpr(filename, parentFullname, _)).mkString(", ")}]"

    val arrayNode = NewArrayInitializer().code(code)
    val arrayAst =
      Ast(arrayNode)
        .withChildren(annotationsAst)
        .withChildren(elemsAst)

    Ast(unknownNode(EmptyAst(), "")).withChild(arrayAst)
  }

  def astForExprAssign(filename: String, parentFullname: String, assignExprInstance: ExprAssign): Ast = {
    val annotationsAst = assignExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toSeq
      case None        => Seq()
    }
    val leftAst = assignExprInstance.left match {
      case Some(left) => Seq(astForExpr(filename, parentFullname, left))
      case None       => Seq()
    }
    val rightAst = assignExprInstance.right match {
      case Some(right) => Seq(astForExpr(filename, parentFullname, right))
      case None        => Seq()
    }

    val leftCode = assignExprInstance.left match {
      case Some(left) => codeForExpr(filename, parentFullname, left)
      case None       => ""
    }
    val rightCode = assignExprInstance.right match {
      case Some(right) => codeForExpr(filename, parentFullname, right)
      case None        => ""
    }
    val code = s"$leftCode = $rightCode"

    val assignAst = unknownNode(assignExprInstance, code)
    Ast(assignAst)
      .withChildren(annotationsAst)
      .withChildren(leftAst)
      .withChildren(rightAst)
  }

  def astForExprAsync(filename: String, parentFullname: String, asyncExprInstance: ExprAsync): Ast = {
    val annotationsAst = asyncExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val stmtAst = asyncExprInstance.stmts.map(astForStmt(filename, parentFullname, _)).toList

    val code = asyncExprInstance.move match {
      case Some(true) => "async move"
      case _          => "async"
    }
    val asyncAst = blockNode(asyncExprInstance).code(code)

    blockAst(asyncAst, stmtAst).withChildren(annotationsAst)
  }

  def astForExprAwait(filename: String, parentFullname: String, awaitExprInstance: ExprAwait): Ast = {
    val annotationsAst = awaitExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toSeq
      case None        => Seq()
    }
    val awaitAst = callNode(awaitExprInstance, ".await", ".await", "", DispatchTypes.STATIC_DISPATCH, None, None)

    awaitExprInstance.base match {
      case Some(base) =>
        val baseAst = astForExpr(filename, parentFullname, base)
        // callAst(awaitAst, Seq(), None, None).withChildren(annotationsAst)
        callAst(awaitAst).withChildren(annotationsAst)
      case None =>
        callAst(awaitAst).withChildren(annotationsAst)
    }
  }

  def astForExprBinary(filename: String, parentFullname: String, binaryExprInstance: ExprBinary): Ast = {
    val annotationsAst = binaryExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toSeq
      case None        => Seq()
    }
    val leftAst = binaryExprInstance.left match {
      case Some(left) => Seq(astForExpr(filename, parentFullname, left))
      case None       => Seq()
    }
    val rightAst = binaryExprInstance.right match {
      case Some(right) => Seq(astForExpr(filename, parentFullname, right))
      case None        => Seq()
    }
    val opAst = binaryExprInstance.op match {
      case Some(op) => Seq(astForBinOp(filename, parentFullname, op))
      case None     => Seq()
    }

    val leftCode = binaryExprInstance.left match {
      case Some(left) => codeForExpr(filename, parentFullname, left)
      case None       => ""
    }
    val rightCode = binaryExprInstance.right match {
      case Some(right) => codeForExpr(filename, parentFullname, right)
      case None        => ""
    }
    val opCode = binaryExprInstance.op match {
      case Some(op) => op.toString
      case None     => ""
    }
    val code = s"$leftCode $opCode $rightCode"

    val binaryAst = unknownNode(binaryExprInstance, code)

    Ast(binaryAst)
      .withChildren(annotationsAst)
      .withChildren(leftAst)
      .withChildren(opAst)
      .withChildren(rightAst)
  }

  def astForExprBlock(filename: String, parentFullname: String, blockExprInstance: ExprBlock): Ast = {
    val annotationsAst = blockExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val labelAst = blockExprInstance.label match {
      case Some(label) => astForLabel(filename, parentFullname, label)
      case None        => Ast()
    }
    val stmtAst = blockExprInstance.stmts.map(astForStmt(filename, parentFullname, _)).toList

    val code = blockExprInstance.label match {
      case Some(label) => s"'${label}: { }"
      case None        => "{ }"
    }

    val exprBlockNode = blockNode(blockExprInstance).code(code)
    blockAst(exprBlockNode, stmtAst)
      .withChild(labelAst)
      .withChildren(annotationsAst)
  }

  def astForExprBreak(filename: String, parentFullname: String, breakExprInstance: ExprBreak): Ast = {
    val annotationsAst = breakExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toSeq
      case None        => Seq()
    }
    val labelAst = breakExprInstance.label match {
      case Some(label) => astForLabel(filename, parentFullname, label)
      case None        => Ast()
    }
    val exprAst = breakExprInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }

    var code = "break"
    code = breakExprInstance.expr match {
      case Some(expr) => s"$code ${codeForExpr(filename, parentFullname, expr)}"
      case None       => code
    }
    code = breakExprInstance.label match {
      case Some(label) => s"break '${label}"
      case None        => code
    }

    val exprBreakNode = controlStructureNode(breakExprInstance, ControlStructureTypes.BREAK, code)
    controlStructureAst(exprBreakNode, None)
      .withChild(labelAst)
      .withChild(exprAst)
      .withChildren(annotationsAst)
  }

  def astForExprCall(filename: String, parentFullname: String, callExprInstance: ExprCall): Ast = {
    val annotationsAst = callExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toSeq
      case None        => Seq()
    }
    val argsAst       = callExprInstance.args.map(astForExpr(filename, parentFullname, _)).toList
    val argWrapperAst = blockAst(blockNode(EmptyAst()), argsAst)

    val code = ""
    val methodFullName = callExprInstance.func match {
      case Some(func) => codeForExpr(filename, parentFullname, func)
      case None       => ""
    }
    val callExprNode =
      callNode(callExprInstance, code, methodFullName, methodFullName, DispatchTypes.STATIC_DISPATCH, None, None)
    callAst(callExprNode, List(argWrapperAst), None, None)
      .withChildren(annotationsAst)
  }

  def astForExprCast(filename: String, parentFullname: String, castExprInstance: ExprCast): Ast = {
    val annotationsAst = castExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toSeq
      case None        => Seq()
    }
    val exprAst = castExprInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }
    val tyAst = castExprInstance.ty match {
      case Some(ty) => astForType(filename, parentFullname, ty)
      case None     => Ast()
    }
    val argWrapperAst = blockAst(blockNode(EmptyAst()), List(exprAst, tyAst))

    var code = ""
    code = castExprInstance.expr match {
      case Some(expr) => s"${codeForExpr(filename, parentFullname, expr)}"
      case None       => ""
    }
    code = castExprInstance.ty match {
      case Some(ty) => s"$code as ${typeFullnameForType(filename, parentFullname, ty)}"
      case None     => code
    }
    val castExprNode =
      callNode(castExprInstance, code, "as", "as", DispatchTypes.STATIC_DISPATCH, None, None)
    callAst(castExprNode, List(argWrapperAst), None, None).withChildren(annotationsAst)
  }

  def astForExprClosure(filename: String, parentFullname: String, closureExprInstance: ExprClosure): Ast = {
    val annotationsAst = closureExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toSeq
      case None        => Seq()
    }

    val bodyAst = closureExprInstance.body match {
      case Some(body) => Seq(astForExpr(filename, parentFullname, body))
      case None       => Seq()
    }
    val closureNode =
      methodNode(closureExprInstance, Defines.Unknown, "", "", filename).isExternal(closureExprInstance.body.isEmpty)
    val inputsAst = closureExprInstance.inputs.map(astForPat(filename, parentFullname, _)).toList
    val methodReturnTypeFullname = closureExprInstance.output match {
      case Some(output) => typeFullnameForType(filename, parentFullname, output)
      case None         => ""
    }
    val methodRetNode = methodReturnNode(closureExprInstance, methodReturnTypeFullname)
    val lifetimeAst =
      closureExprInstance.lifetimes match {
        case Some(lifetimes) => lifetimes.map(astForGenericParam(filename, parentFullname, _)).toSeq
        case None            => Seq()
      }

    methodAstWithAnnotations(closureNode, inputsAst, bodyAst.head, methodRetNode, Nil, annotationsAst).withChildren(
      lifetimeAst
    )
  }

  def astForExprConst(filename: String, parentFullname: String, constExprInstance: ExprConst): Ast = {
    val annotationsAst = constExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val stmtsAst = constExprInstance.stmts.map(astForStmt(filename, parentFullname, _)).toList

    val constNode = blockNode(constExprInstance).code("const")
    blockAst(constNode, stmtsAst)
      .withChildren(annotationsAst)
  }

  def astForExprContinue(filename: String, parentFullname: String, continueExprInstance: ExprContinue): Ast = {
    val annotationsAst = continueExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toSeq
      case None        => Seq()
    }
    val labelAst = continueExprInstance.label match {
      case Some(label) => astForLabel(filename, parentFullname, label)
      case None        => Ast()
    }

    val code = continueExprInstance.label match {
      case Some(label) => s"continue '${label}"
      case None        => "continue"
    }
    val exprContinueNode = controlStructureNode(continueExprInstance, ControlStructureTypes.CONTINUE, "continue")

    controlStructureAst(exprContinueNode, None)
      .withChild(labelAst)
      .withChildren(annotationsAst)
  }

  def astForExprField(filename: String, parentFullname: String, fieldExprInstance: ExprField): Ast = {
    val annotationsAst = fieldExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val baseName = fieldExprInstance.base match {
      case Some(base) => codeForExpr(filename, parentFullname, base)
      case None       => ""
    }
    var ident = fieldExprInstance.named match {
      case Some(name) => name
      case None       => Defines.Unknown
    }
    ident = fieldExprInstance.unnamed match {
      case Some(index) => index.toString
      case None        => ident
    }
    val name = s"$baseName.$ident"

    val fieldNode = fieldIdentifierNode(fieldExprInstance, name, name)
    Ast(fieldNode)
    // .withChildren(annotationsAst)
  }

  def astForExprForLoop(filename: String, parentFullname: String, forLoopExprInstance: ExprForLoop): Ast = {
    val annotationsAst = forLoopExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val labelAst = forLoopExprInstance.label match {
      case Some(label) => astForLabel(filename, parentFullname, label)
      case None        => Ast()
    }
    val patAst = forLoopExprInstance.pat match {
      case Some(pat) => astForPat(filename, parentFullname, pat)
      case None      => Ast()
    }
    val pathWrapperAst = blockAst(blockNode(EmptyAst()), List(patAst))
    val exprAst = forLoopExprInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }
    val exprWrapperAst = blockAst(blockNode(EmptyAst()), List(exprAst))
    val bodyAst        = astForBlock(filename, parentFullname, forLoopExprInstance.body)

    val forLoopNode = controlStructureNode(forLoopExprInstance, ControlStructureTypes.FOR, "")

    forAst(forLoopNode, Seq(pathWrapperAst), Seq(pathWrapperAst), Seq(exprWrapperAst), Seq(exprWrapperAst), bodyAst)
      .withChild(labelAst)
      .withChildren(annotationsAst)
  }

  def astForExprGroup(filename: String, parentFullname: String, groupExprInstance: ExprGroup): Ast = {
    val annotationsAst = groupExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val exprAst = groupExprInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }
    val groupNode = unknownNode(groupExprInstance, "")

    Ast(groupNode)
      .withChild(exprAst)
      .withChildren(annotationsAst)
  }

  def astForExprIf(filename: String, parentFullname: String, ifExprInstance: ExprIf): Ast = {
    val annotationsAst = ifExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val condAst = ifExprInstance.cond match {
      case Some(cond) => astForExpr(filename, parentFullname, cond)
      case None       => Ast()
    }
    val condWrapperAst = blockAst(blockNode(EmptyAst()), List(condAst))
    val thenAst        = astForBlock(filename, parentFullname, ifExprInstance.then_branch)
    val elseAst = ifExprInstance.else_branch match {
      case Some(elseBranch) => astForExpr(filename, parentFullname, elseBranch)
      case None             => Ast()
    }

    val exprIfNode = controlStructureNode(ifExprInstance, ControlStructureTypes.IF, "")

    controlStructureAst(exprIfNode, Some(condWrapperAst))
      .withChild(thenAst)
      .withChild(elseAst)
      .withChildren(annotationsAst)
  }

  def astForExprIndex(filename: String, parentFullname: String, indexExprInstance: ExprIndex): Ast = {
    val annotationsAst = indexExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val exprCode = indexExprInstance.expr match {
      case Some(expr) => codeForExpr(filename, parentFullname, expr)
      case None       => Defines.Unknown
    }
    val indexCode = indexExprInstance.index match {
      case Some(index) => codeForExpr(filename, parentFullname, index)
      case None        => Defines.Unknown
    }
    val name = s"$exprCode[$indexCode]"

    val indexExprNode = fieldIdentifierNode(indexExprInstance, name, name)
    Ast(indexExprNode)
    // .withChildren(annotationsAst)
  }

  def astForExprInfer(filename: String, parentFullname: String, inferExprInstance: ExprInfer): Ast = {
    val annotationsAst = inferExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val exprInfAst = unknownNode(inferExprInstance, "")

    Ast(exprInfAst)
      .withChildren(annotationsAst)
  }

  def astForExprLet(filename: String, parentFullname: String, letExprInstance: ExprLet): Ast = {
    val annotationsAst = letExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val patAst = letExprInstance.pat match {
      case Some(pat) => astForPat(filename, parentFullname, pat)
      case None      => Ast()
    }
    val exprAst = letExprInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }

    val exprLetAst = unknownNode(letExprInstance, "")

    Ast(exprLetAst)
      .withChild(patAst)
      .withChild(exprAst)
      .withChildren(annotationsAst)
  }

  def astForExprLit(filename: String, parentFullname: String, litExprInstance: ExprLit): Ast = {
    val annotationsAst = litExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val litInstance = Lit(
      litExprInstance.strLit,
      litExprInstance.byteStrLit,
      litExprInstance.byteLit,
      litExprInstance.charLit,
      litExprInstance.intLit,
      litExprInstance.floatLit,
      litExprInstance.boolLit,
      litExprInstance.verbatimLit
    )
    val exprLitAst = astForLit(filename, parentFullname, litInstance)

    exprLitAst.withChildren(annotationsAst)
  }

  def astForExprLoop(filename: String, parentFullname: String, loopExprInstance: ExprLoop): Ast = {
    val annotationsAst = loopExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val labelAst = loopExprInstance.label match {
      case Some(label) => astForLabel(filename, parentFullname, label)
      case None        => Ast()
    }
    val bodyAst = astForBlock(filename, parentFullname, loopExprInstance.body)

    val loopNode = controlStructureNode(loopExprInstance, ControlStructureTypes.DO, "")

    controlStructureAst(loopNode, None)
      .withChild(labelAst)
      .withChild(bodyAst)
      .withChildren(annotationsAst)
  }

  def astForExprMacro(filename: String, parentFullname: String, macroExprInstance: ExprMacro): Ast = {
    val annotationsAst = macroExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val macroRustAst = Macro(macroExprInstance.path, macroExprInstance.delimiter, macroExprInstance.tokens)
    astForMacro(filename, parentFullname, macroRustAst).withChildren(annotationsAst)
  }

  def astForExprMatch(filename: String, parentFullname: String, matchExprInstance: ExprMatch): Ast = {
    val annotationsAst = matchExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val exprAst = matchExprInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }
    val condWrapperAst = blockAst(blockNode(EmptyAst()), List(exprAst))
    val armsAst        = matchExprInstance.arms.map(astForArm(filename, parentFullname, _)).toList

    val exprMatchAst = controlStructureNode(matchExprInstance, ControlStructureTypes.MATCH, "")

    controlStructureAst(exprMatchAst, Some(condWrapperAst))
      .withChildren(armsAst)
      .withChildren(annotationsAst)
  }

  def astForExprMethodCall(filename: String, parentFullname: String, methodCallExprInstance: ExprMethodCall): Ast = {
    val annotationsAst = methodCallExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val receiverAst = methodCallExprInstance.receiver match {
      case Some(receiver) => astForExpr(filename, parentFullname, receiver)
      case None           => Ast()
    }
    val receiverWrapperAst = blockAst(blockNode(EmptyAst()), List(receiverAst))
    val turbofishAst = methodCallExprInstance.turbofish match {
      case Some(turbofish) => astForAngleBracketedGenericArguments(filename, parentFullname, turbofish)
      case None            => Ast()
    }
    val argsAst       = methodCallExprInstance.args.map(astForExpr(filename, parentFullname, _)).toList
    val argWrapperAst = blockAst(blockNode(EmptyAst()), argsAst)

    val exprMethodCallAst = callNode(
      methodCallExprInstance,
      "",
      methodCallExprInstance.method,
      methodCallExprInstance.method,
      DispatchTypes.STATIC_DISPATCH,
      None,
      None
    )

    callAst(exprMethodCallAst, List(argWrapperAst), None, Some(receiverWrapperAst))
      .withChild(turbofishAst)
      .withChildren(annotationsAst)
  }

  def astForExprParen(filename: String, parentFullname: String, parenExprInstance: ExprParen): Ast = {
    val annotationsAst = parenExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val exprAst = parenExprInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }
    val exprParenNode = unknownNode(parenExprInstance, "")

    Ast(exprParenNode)
      .withChild(exprAst)
      .withChildren(annotationsAst)
  }

  def astForExprPath(filename: String, parentFullname: String, pathExprInstance: ExprPath): Ast = {
    val annotationsAst = pathExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val path    = Path(pathExprInstance.segments, pathExprInstance.leading_colon)
    val pathAst = astForPath(filename, parentFullname, path)
    val qselfAst = pathExprInstance.qself match {
      case Some(qself) => List(astForQself(filename, parentFullname, qself))
      case None        => List()
    }
    val exprPathNode = identifierNode(pathExprInstance, "", "", "")
    val exprPathAst  = Ast(exprPathNode)

    Ast(unknownNode(EmptyAst(), ""))
      .withChild(exprPathAst)
      .withChildren(annotationsAst)
  }

  def astForExprRange(filename: String, parentFullname: String, rangeExprInstance: ExprRange): Ast = {
    val annotationsAst = rangeExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val startAst = rangeExprInstance.start match {
      case Some(start) => astForExpr(filename, parentFullname, start)
      case None        => Ast()
    }
    val endAst = rangeExprInstance.end match {
      case Some(end) => astForExpr(filename, parentFullname, end)
      case None      => Ast()
    }
    val limitAst = rangeExprInstance.limits match {
      case Some(limit) => astForRangeLimits(filename, parentFullname, limit)
      case None        => Ast()
    }

    val exprRangeNode = NewArrayInitializer()
    val exprRangeAst =
      Ast(exprRangeNode)
        .withChildren(annotationsAst)
        .withChild(startAst)
        .withChild(limitAst)
        .withChild(endAst)

    Ast(unknownNode(EmptyAst(), ""))
      .withChild(exprRangeAst)
  }

  def astForExprReference(filename: String, parentFullname: String, referenceExprInstance: ExprReference): Ast = {
    val annotationsAst = referenceExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val typeFullname = referenceExprInstance.expr match {
      case Some(expr) => codeForExpr(filename, parentFullname, expr)
      case None       => Defines.Unknown
    }
    val code = referenceExprInstance.mut match {
      case Some(true) => s"&mut ${typeFullname}"
      case _          => s"&${typeFullname}"
    }
    val exprReferenceAst = typeRefNode(referenceExprInstance, code, typeFullname)

    Ast(exprReferenceAst)
    // .withChildren(annotationsAst)
  }

  def astForExprRepeat(filename: String, parentFullname: String, repeatExprInstance: ExprRepeat): Ast = {
    val annotationsAst = repeatExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val exprCode = repeatExprInstance.expr match {
      case Some(expr) => codeForExpr(filename, parentFullname, expr)
      case None       => Defines.Unknown
    }
    val lenCode = repeatExprInstance.len match {
      case Some(len) => codeForExpr(filename, parentFullname, len)
      case None      => Defines.Unknown
    }
    val code           = s"[$exprCode; $lenCode]"
    val exprRepeatNode = NewArrayInitializer().code(code)

    val exprRepeateAst = Ast(exprRepeatNode)
      .withChildren(annotationsAst)

    Ast(unknownNode(EmptyAst(), "")).withChild(exprRepeateAst)
  }

  def astForExprReturn(filename: String, parentFullname: String, returnExprInstance: ExprReturn): Ast = {
    val annotationsAst = returnExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val exprAst = returnExprInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }
    val exprReturnCode = returnExprInstance.expr match {
      case Some(expr) => codeForExpr(filename, parentFullname, expr)
      case None       => Defines.Unknown
    }
    val code = s"return $exprReturnCode"

    val exprReturnNode = returnNode(returnExprInstance, code)

    returnAst(exprReturnNode, Seq(exprAst))
      .withChildren(annotationsAst)
    // .withChildren(exprAst)
  }

  def astForExprStruct(filename: String, parentFullname: String, structExprInstance: ExprStruct): Ast = {
    val annotationsAst = structExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val qselfAst = structExprInstance.qself match {
      case Some(qself) => astForQself(filename, parentFullname, qself)
      case None        => Ast()
    }
    val pathAst = structExprInstance.path match {
      case Some(path) => astForPath(filename, parentFullname, path)
      case None       => Ast()
    }
    val fieldsAst = structExprInstance.fields.map(astForFieldValue(filename, parentFullname, _)).toList
    val restAst = structExprInstance.rest match {
      case Some(rest) => astForExpr(filename, parentFullname, rest)
      case None       => Ast()
    }

    val exprStructAst = unknownNode(structExprInstance, "")

    Ast(exprStructAst)
      .withChildren(annotationsAst)
      .withChild(qselfAst)
      .withChild(pathAst)
      .withChildren(fieldsAst)
      .withChild(restAst)
  }

  def astForExprTry(filename: String, parentFullname: String, tryExprInstance: ExprTry): Ast = {
    val annotationsAst = tryExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val exprAst = tryExprInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }

    val exprTryAst = controlStructureNode(tryExprInstance, ControlStructureTypes.THROW, "")

    controlStructureAst(exprTryAst, None)
      .withChild(exprAst)
      .withChildren(annotationsAst)
  }

  def astForExprTryBlock(filename: String, parentFullname: String, tryBlockExprInstance: ExprTryBlock): Ast = {
    val annotationsAst = tryBlockExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val stmtsAst = tryBlockExprInstance.stmts.map(astForStmt(filename, parentFullname, _)).toList

    val exprTryBlockAst = blockNode(tryBlockExprInstance)

    blockAst(exprTryBlockAst, stmtsAst)
      .withChildren(annotationsAst)
  }

  def astForExprTuple(filename: String, parentFullname: String, tupleExprInstance: ExprTuple): Ast = {
    val annotationsAst = tupleExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }
    val elemsAst = tupleExprInstance.elems.map(astForExpr(filename, parentFullname, _)).toList
    val code     = s"(${tupleExprInstance.elems.map(codeForExpr(filename, parentFullname, _)).mkString(", ")})"

    val exprTupleNode = NewArrayInitializer().code(code)
    val exprTupleAst = Ast(exprTupleNode)
      .withChildren(annotationsAst)
      .withChildren(elemsAst)

    Ast(unknownNode(EmptyAst(), ""))
      .withChild(exprTupleAst)
  }

  def astForExprUnary(filename: String, parentFullname: String, unaryExprInstance: ExprUnary): Ast = {
    val annotationsAst = unaryExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val exprAst = unaryExprInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }
    val opAst = unaryExprInstance.op match {
      case Some(op) => astForUnOp(filename, parentFullname, op)
      case None     => Ast()
    }
    val exprUnaryNode = unknownNode(unaryExprInstance, "")

    Ast(exprUnaryNode)
      .withChild(opAst)
      .withChild(exprAst)
      .withChildren(annotationsAst)
  }

  def astForExprUnsafe(filename: String, parentFullname: String, unsafeExprInstance: ExprUnsafe): Ast = {
    val annotationsAst = unsafeExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val stmtsAst = unsafeExprInstance.stmts.map(astForStmt(filename, parentFullname, _)).toList

    val exprUnsafeBlockAst = blockNode(unsafeExprInstance)

    blockAst(exprUnsafeBlockAst, stmtsAst)
      .withChildren(annotationsAst)
  }

  def astForExprWhile(filename: String, parentFullname: String, whileExprInstance: ExprWhile): Ast = {
    val annotationsAst = whileExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val labelAst = whileExprInstance.label match {
      case Some(label) => astForLabel(filename, parentFullname, label)
      case None        => Ast()
    }
    val condAst = whileExprInstance.cond match {
      case Some(cond) => astForExpr(filename, parentFullname, cond)
      case None       => Ast()
    }
    val bodyAst = whileExprInstance.body.map(astForStmt(filename, parentFullname, _)).toList

    whileAst(Some(condAst), bodyAst)
      .withChild(labelAst)
      .withChildren(annotationsAst)
  }

  def astForExprYield(filename: String, parentFullname: String, yieldExprInstance: ExprYield): Ast = {
    val annotationsAst = yieldExprInstance.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
      case None        => List()
    }

    val exprAst = yieldExprInstance.expr match {
      case Some(expr) => astForExpr(filename, parentFullname, expr)
      case None       => Ast()
    }
    val exprCode = yieldExprInstance.expr match {
      case Some(expr) => codeForExpr(filename, parentFullname, expr)
      case None       => Defines.Unknown
    }
    val code = s"yield $exprCode"

    val exprYieldAst = returnNode(yieldExprInstance, code)

    returnAst(exprYieldAst, Seq(exprAst))
      .withChildren(annotationsAst)
    // .withChildren(exprAst)
  }
}

trait CodeForExpr(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def codeForExpr(filename: String, parentFullname: String, exprInstance: Expr): String = {
    if (exprInstance.arrayExpr.isDefined) {
      codeForExprArray(filename, parentFullname, exprInstance.arrayExpr.get)
    } else if (exprInstance.assignExpr.isDefined) {
      codeForExprAssign(filename, parentFullname, exprInstance.assignExpr.get)
    } else if (exprInstance.asyncExpr.isDefined) {
      codeForExprAsync(filename, parentFullname, exprInstance.asyncExpr.get)
    } else if (exprInstance.awaitExpr.isDefined) {
      codeForExprAwait(filename, parentFullname, exprInstance.awaitExpr.get)
    } else if (exprInstance.binaryExpr.isDefined) {
      codeForExprBinary(filename, parentFullname, exprInstance.binaryExpr.get)
    } else if (exprInstance.blockExpr.isDefined) {
      codeForExprBlock(filename, parentFullname, exprInstance.blockExpr.get)
    } else if (exprInstance.breakExpr.isDefined) {
      codeForExprBreak(filename, parentFullname, exprInstance.breakExpr.get)
    } else if (exprInstance.callExpr.isDefined) {
      codeForExprCall(filename, parentFullname, exprInstance.callExpr.get)
    } else if (exprInstance.castExpr.isDefined) {
      codeForExprCast(filename, parentFullname, exprInstance.castExpr.get)
    } else if (exprInstance.closureExpr.isDefined) {
      codeForExprClosure(filename, parentFullname, exprInstance.closureExpr.get)
    } else if (exprInstance.constExpr.isDefined) {
      codeForExprConst(filename, parentFullname, exprInstance.constExpr.get)
    } else if (exprInstance.continueExpr.isDefined) {
      codeForExprContinue(filename, parentFullname, exprInstance.continueExpr.get)
    } else if (exprInstance.fieldExpr.isDefined) {
      codeForExprField(filename, parentFullname, exprInstance.fieldExpr.get)
    } else if (exprInstance.forLoopExpr.isDefined) {
      codeForExprForLoop(filename, parentFullname, exprInstance.forLoopExpr.get)
    } else if (exprInstance.groupExpr.isDefined) {
      codeForExprGroup(filename, parentFullname, exprInstance.groupExpr.get)
    } else if (exprInstance.ifExpr.isDefined) {
      codeForExprIf(filename, parentFullname, exprInstance.ifExpr.get)
    } else if (exprInstance.indexExpr.isDefined) {
      codeForExprIndex(filename, parentFullname, exprInstance.indexExpr.get)
    } else if (exprInstance.inferExpr.isDefined) {
      codeForExprInfer(filename, parentFullname, exprInstance.inferExpr.get)
    } else if (exprInstance.letExpr.isDefined) {
      codeForExprLet(filename, parentFullname, exprInstance.letExpr.get)
    } else if (exprInstance.litExpr.isDefined) {
      codeForExprLit(filename, parentFullname, exprInstance.litExpr.get)
    } else if (exprInstance.loopExpr.isDefined) {
      codeForExprLoop(filename, parentFullname, exprInstance.loopExpr.get)
    } else if (exprInstance.macroExpr.isDefined) {
      codeForExprMacro(filename, parentFullname, exprInstance.macroExpr.get)
    } else if (exprInstance.matchExpr.isDefined) {
      codeForExprMatch(filename, parentFullname, exprInstance.matchExpr.get)
    } else if (exprInstance.methodCallExpr.isDefined) {
      codeForExprMethodCall(filename, parentFullname, exprInstance.methodCallExpr.get)
    } else if (exprInstance.parenExpr.isDefined) {
      codeForExprParen(filename, parentFullname, exprInstance.parenExpr.get)
    } else if (exprInstance.pathExpr.isDefined) {
      codeForExprPath(filename, parentFullname, exprInstance.pathExpr.get)
    } else if (exprInstance.rangeExpr.isDefined) {
      codeForExprRange(filename, parentFullname, exprInstance.rangeExpr.get)
    } else if (exprInstance.referenceExpr.isDefined) {
      codeForExprReference(filename, parentFullname, exprInstance.referenceExpr.get)
    } else if (exprInstance.repeatExpr.isDefined) {
      codeForExprRepeat(filename, parentFullname, exprInstance.repeatExpr.get)
    } else if (exprInstance.returnExpr.isDefined) {
      codeForExprReturn(filename, parentFullname, exprInstance.returnExpr.get)
    } else if (exprInstance.structExpr.isDefined) {
      codeForExprStruct(filename, parentFullname, exprInstance.structExpr.get)
    } else if (exprInstance.tryExpr.isDefined) {
      codeForExprTry(filename, parentFullname, exprInstance.tryExpr.get)
    } else if (exprInstance.tryBlockExpr.isDefined) {
      codeForExprTryBlock(filename, parentFullname, exprInstance.tryBlockExpr.get)
    } else if (exprInstance.tupleExpr.isDefined) {
      codeForExprTuple(filename, parentFullname, exprInstance.tupleExpr.get)
    } else if (exprInstance.unaryExpr.isDefined) {
      codeForExprUnary(filename, parentFullname, exprInstance.unaryExpr.get)
    } else if (exprInstance.unsafeExpr.isDefined) {
      codeForExprUnsafe(filename, parentFullname, exprInstance.unsafeExpr.get)
    } else if (exprInstance.verbatimExpr.isDefined) {
      codeForTokenStream(filename, parentFullname, exprInstance.verbatimExpr.get)
    } else if (exprInstance.whileExpr.isDefined) {
      codeForExprWhile(filename, parentFullname, exprInstance.whileExpr.get)
    } else if (exprInstance.yieldExpr.isDefined) {
      codeForExprYield(filename, parentFullname, exprInstance.yieldExpr.get)
    } else {
      throw new IllegalArgumentException("Unsupported expression type")
    }
  }

  def codeForExprArray(filename: String, parentFullname: String, arrayExprInstance: ExprArray): String = {
    "ArrayExpr"
  }
  def codeForExprAssign(filename: String, parentFullname: String, assignExprInstance: ExprAssign): String = {
    "AssignExpr"
  }
  def codeForExprAsync(filename: String, parentFullname: String, asyncExprInstance: ExprAsync): String = {
    "AsyncExpr"
  }
  def codeForExprAwait(filename: String, parentFullname: String, awaitExprInstance: ExprAwait): String = {
    "AwaitExpr"
  }
  def codeForExprBinary(filename: String, parentFullname: String, binaryExprInstance: ExprBinary): String = {
    "BinaryExpr"
  }
  def codeForExprBlock(filename: String, parentFullname: String, blockExprInstance: ExprBlock): String = {
    "BlockExpr"
  }
  def codeForExprBreak(filename: String, parentFullname: String, breakExprInstance: ExprBreak): String = {
    "BreakExpr"
  }
  def codeForExprCall(filename: String, parentFullname: String, callExprInstance: ExprCall): String = {
    "CallExpr"
  }
  def codeForExprCast(filename: String, parentFullname: String, castExprInstance: ExprCast): String = {
    "CastExpr"
  }
  def codeForExprClosure(filename: String, parentFullname: String, closureExprInstance: ExprClosure): String = {
    "ClosureExpr"
  }
  def codeForExprConst(filename: String, parentFullname: String, constExprInstance: ExprConst): String = {
    "ConstExpr"
  }
  def codeForExprContinue(filename: String, parentFullname: String, continueExprInstance: ExprContinue): String = {
    "ContinueExpr"
  }
  def codeForExprField(filename: String, parentFullname: String, fieldExprInstance: ExprField): String = {
    "FieldExpr"
  }
  def codeForExprForLoop(filename: String, parentFullname: String, forLoopExprInstance: ExprForLoop): String = {
    "ForLoopExpr"
  }
  def codeForExprGroup(filename: String, parentFullname: String, groupExprInstance: ExprGroup): String = {
    "GroupExpr"
  }
  def codeForExprIf(filename: String, parentFullname: String, ifExprInstance: ExprIf): String = {
    "IfExpr"
  }
  def codeForExprIndex(filename: String, parentFullname: String, indexExprInstance: ExprIndex): String = {
    "IndexExpr"
  }
  def codeForExprInfer(filename: String, parentFullname: String, inferExprInstance: ExprInfer): String = {
    "InferExpr"
  }
  def codeForExprLet(filename: String, parentFullname: String, letExprInstance: ExprLet): String = {
    "LetExpr"
  }
  def codeForExprLit(filename: String, parentFullname: String, litExprInstance: ExprLit): String = {
    "LitExpr"
  }
  def codeForExprLoop(filename: String, parentFullname: String, loopExprInstance: ExprLoop): String = {
    "LoopExpr"
  }
  def codeForExprMacro(filename: String, parentFullname: String, macroExprInstance: ExprMacro): String = {
    "MacroExpr"
  }
  def codeForExprMatch(filename: String, parentFullname: String, matchExprInstance: ExprMatch): String = {
    "MatchExpr"
  }
  def codeForExprMethodCall(
    filename: String,
    parentFullname: String,
    methodCallExprInstance: ExprMethodCall
  ): String = {
    "MethodCallExpr"
  }
  def codeForExprParen(filename: String, parentFullname: String, parenExprInstance: ExprParen): String = {
    "ParenExpr"
  }
  def codeForExprPath(filename: String, parentFullname: String, pathExprInstance: ExprPath): String = {
    "PathExpr"
  }
  def codeForExprRange(filename: String, parentFullname: String, rangeExprInstance: ExprRange): String = {
    "RangeExpr"
  }
  def codeForExprReference(filename: String, parentFullname: String, referenceExprInstance: ExprReference): String = {
    "ReferenceExpr"
  }
  def codeForExprRepeat(filename: String, parentFullname: String, repeatExprInstance: ExprRepeat): String = {
    "RepeatExpr"
  }
  def codeForExprReturn(filename: String, parentFullname: String, returnExprInstance: ExprReturn): String = {
    "ReturnExpr"
  }
  def codeForExprStruct(filename: String, parentFullname: String, structExprInstance: ExprStruct): String = {
    "StructExpr"
  }
  def codeForExprTry(filename: String, parentFullname: String, tryExprInstance: ExprTry): String = {
    "TryExpr"
  }
  def codeForExprTryBlock(filename: String, parentFullname: String, tryBlockExprInstance: ExprTryBlock): String = {
    "TryBlockExpr"
  }
  def codeForExprTuple(filename: String, parentFullname: String, tupleExprInstance: ExprTuple): String = {
    "TupleExpr"
  }
  def codeForExprUnary(filename: String, parentFullname: String, unaryExprInstance: ExprUnary): String = {
    "UnaryExpr"
  }
  def codeForExprUnsafe(filename: String, parentFullname: String, unsafeExprInstance: ExprUnsafe): String = {
    "UnsafeExpr"
  }
  def codeForExprWhile(filename: String, parentFullname: String, whileExprInstance: ExprWhile): String = {
    "WhileExpr"
  }
  def codeForExprYield(filename: String, parentFullname: String, yieldExprInstance: ExprYield): String = {
    "YieldExpr"
  }
}
