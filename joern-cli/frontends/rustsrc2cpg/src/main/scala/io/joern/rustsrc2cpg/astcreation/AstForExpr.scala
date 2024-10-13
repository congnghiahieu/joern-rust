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
    val annotationsAst = arrayExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val elemsAst       = arrayExprInstance.elems.map(astForExpr(filename, parentFullname, _)).toList

    val astArray = NewArrayInitializer()
    Ast(astArray)
      .withChildren(annotationsAst)
      .withChildren(elemsAst)
  }

  def astForExprAssign(filename: String, parentFullname: String, assignExprInstance: ExprAssign): Ast = {
    val annotationsAst = assignExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val leftAst        = assignExprInstance.left.toList.map(astForExpr(filename, parentFullname, _))
    val rightAst       = assignExprInstance.right.toList.map(astForExpr(filename, parentFullname, _))

    val assignAst = NewUnknown()
    Ast(assignAst)
      .withChildren(annotationsAst)
      .withChildren(leftAst)
      .withChildren(rightAst)
  }

  def astForExprAsync(filename: String, parentFullname: String, asyncExprInstance: ExprAsync): Ast = {
    val annotationsAst = asyncExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val stmtAst        = astForBlock(filename, parentFullname, asyncExprInstance.stmts)

    val asyncAst = NewBlock()
    Ast(asyncAst)
      .withChildren(annotationsAst)
      .withChild(stmtAst)
  }

  def astForExprAwait(filename: String, parentFullname: String, awaitExprInstance: ExprAwait): Ast = {
    val annotationsAst = awaitExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val awaitAst       = NewCall()

    awaitExprInstance.base match {
      case Some(base) =>
        val baseAst = astForExpr(filename, parentFullname, base)
        Ast(awaitAst)
          .withChildren(annotationsAst)
          .withChild(baseAst)
      case None =>
        Ast(awaitAst)
          .withChildren(annotationsAst)
    }
  }

  def astForExprBinary(filename: String, parentFullname: String, binaryExprInstance: ExprBinary): Ast = {
    val annotationsAst = binaryExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val leftAst        = binaryExprInstance.left.toList.map(astForExpr(filename, parentFullname, _))
    val rightAst       = binaryExprInstance.right.toList.map(astForExpr(filename, parentFullname, _))
    val opAst          = binaryExprInstance.op.toList.map(astForBinOp(filename, parentFullname, _))

    val binaryAst = NewUnknown()
    Ast(binaryAst)
      .withChildren(annotationsAst)
      .withChildren(leftAst)
      .withChildren(opAst)
      .withChildren(rightAst)
  }

  def astForExprBlock(filename: String, parentFullname: String, blockExprInstance: ExprBlock): Ast = {
    val annotationsAst = blockExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val labelAst       = blockExprInstance.label.toList.map(astForLabel(filename, parentFullname, _))
    val stmtAst        = astForBlock(filename, parentFullname, blockExprInstance.stmts)

    val exprBlockAst = NewBlock()
    Ast(exprBlockAst)
      .withChildren(annotationsAst)
      .withChildren(labelAst)
      .withChild(stmtAst)
  }

  def astForExprBreak(filename: String, parentFullname: String, breakExprInstance: ExprBreak): Ast = {
    val annotationsAst = breakExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val labelAst       = breakExprInstance.label.toList.map(astForLabel(filename, parentFullname, _))
    val exprAst        = breakExprInstance.expr.toList.map(astForExpr(filename, parentFullname, _))

    val exprBreakAst = NewControlStructure()
      .controlStructureType(ControlStructureTypes.BREAK)
      .parserTypeName(classOf[ExprBreak].getSimpleName)

    Ast(exprBreakAst)
      .withChildren(annotationsAst)
      .withChildren(labelAst)
      .withChildren(exprAst)
  }

  def astForExprCall(filename: String, parentFullname: String, callExprInstance: ExprCall): Ast = {
    val annotationsAst = callExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val funcAst        = callExprInstance.func.toList.map(astForExpr(filename, parentFullname, _))
    val argsAst        = callExprInstance.args.map(astForExpr(filename, parentFullname, _)).toList

    val exprCallAst = NewCall()
    Ast(exprCallAst)
      .withChildren(annotationsAst)
      .withChildren(funcAst)
      .withChildren(argsAst)
  }

  def astForExprCast(filename: String, parentFullname: String, castExprInstance: ExprCast): Ast = {
    val annotationsAst = castExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val exprAst        = castExprInstance.expr.toList.map(astForExpr(filename, parentFullname, _))
    val tyAst          = castExprInstance.ty.toList.map(astForType(filename, parentFullname, _))

    val exprCastAst = NewCall()
    Ast(exprCastAst)
      .withChildren(annotationsAst)
      .withChildren(exprAst)
      .withChildren(tyAst)
  }

  def astForExprClosure(filename: String, parentFullname: String, closureExprInstance: ExprClosure): Ast = {
    val annotationsAst = closureExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val bodyAst        = closureExprInstance.body.toList.map(astForExpr(filename, parentFullname, _))
    val inputsAst      = closureExprInstance.inputs.map(astForPat(filename, parentFullname, _)).toList
    val outputAst      = closureExprInstance.output.toList.map(astForType(filename, parentFullname, _))
    val lifetimeAst =
      closureExprInstance.lifetimes.toList.flatMap(_.map(astForGenericParam(filename, parentFullname, _)))

    val closureAst = NewMethod()
    Ast(closureAst)
      .withChildren(annotationsAst)
      .withChildren(inputsAst)
      .withChildren(outputAst)
      .withChildren(bodyAst)
      .withChildren(lifetimeAst)
  }

  def astForExprConst(filename: String, parentFullname: String, constExprInstance: ExprConst): Ast = {
    val annotationsAst = constExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val stmtsAst       = astForBlock(filename, parentFullname, constExprInstance.stmts)

    val constAst = NewLocal()
    Ast(constAst)
      .withChildren(annotationsAst)
      .withChild(stmtsAst)
  }

  def astForExprContinue(filename: String, parentFullname: String, continueExprInstance: ExprContinue): Ast = {
    val annotationsAst = continueExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val labelAst       = continueExprInstance.label.toList.map(astForLabel(filename, parentFullname, _))

    val continueAst = NewControlStructure()
      .controlStructureType(ControlStructureTypes.CONTINUE)
      .parserTypeName(classOf[ExprContinue].getSimpleName)

    Ast(continueAst)
      .withChildren(annotationsAst)
      .withChildren(labelAst)
  }

  def astForExprField(filename: String, parentFullname: String, fieldExprInstance: ExprField): Ast = {
    val annotationsAst = fieldExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val baseAst        = fieldExprInstance.base.toList.flatMap(base => List(astForExpr(filename, parentFullname, base)))

    val fieldAst = NewFieldIdentifier()

    Ast(fieldAst)
      .withChildren(annotationsAst)
      .withChildren(baseAst)
  }

  def astForExprForLoop(filename: String, parentFullname: String, forLoopExprInstance: ExprForLoop): Ast = {
    val annotationsAst = forLoopExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val labelAst = forLoopExprInstance.label.toList.flatMap(label => List(astForLabel(filename, parentFullname, label)))
    val patAst   = forLoopExprInstance.pat.toList.flatMap(pat => List(astForPat(filename, parentFullname, pat)))
    val exprAst  = forLoopExprInstance.expr.toList.flatMap(expr => List(astForExpr(filename, parentFullname, expr)))
    val bodyAst  = astForBlock(filename, parentFullname, forLoopExprInstance.body)

    val forLoopAst = NewControlStructure()
      .controlStructureType(ControlStructureTypes.FOR)
      .parserTypeName(classOf[ExprForLoop].getSimpleName)

    Ast(forLoopAst)
      .withChildren(annotationsAst)
      .withChildren(labelAst)
      .withChildren(patAst)
      .withChildren(exprAst)
      .withChild(bodyAst)
  }

  def astForExprGroup(filename: String, parentFullname: String, groupExprInstance: ExprGroup): Ast = {
    val annotationsAst = groupExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val exprAst        = groupExprInstance.expr.toList.flatMap(expr => List(astForExpr(filename, parentFullname, expr)))

    val groupAst = NewUnknown()

    Ast(groupAst)
      .withChildren(annotationsAst)
      .withChildren(exprAst)
  }

  def astForExprIf(filename: String, parentFullname: String, ifExprInstance: ExprIf): Ast = {
    val annotationsAst = ifExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val condAst        = ifExprInstance.cond.toList.flatMap(cond => List(astForExpr(filename, parentFullname, cond)))
    val thenAst        = astForBlock(filename, parentFullname, ifExprInstance.then_branch)
    val elseAst =
      ifExprInstance.else_branch.toList.flatMap(elseBranch => List(astForExpr(filename, parentFullname, elseBranch)))

    val ifAst = NewControlStructure()
      .controlStructureType(ControlStructureTypes.IF)
      .parserTypeName(classOf[ExprIf].getSimpleName)

    Ast(ifAst)
      .withChildren(annotationsAst)
      .withChildren(condAst)
      .withChild(thenAst)
      .withChildren(elseAst)
  }

  def astForExprIndex(filename: String, parentFullname: String, indexExprInstance: ExprIndex): Ast = {
    val annotationsAst = indexExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val exprAst        = indexExprInstance.expr.toList.flatMap(expr => List(astForExpr(filename, parentFullname, expr)))
    val indexAst = indexExprInstance.index.toList.flatMap(index => List(astForExpr(filename, parentFullname, index)))

    val indexExprAst = NewFieldIdentifier()

    Ast(indexExprAst)
      .withChildren(annotationsAst)
      .withChildren(exprAst)
      .withChildren(indexAst)
  }

  def astForExprInfer(filename: String, parentFullname: String, inferExprInstance: ExprInfer): Ast = {
    val annotationsAst = inferExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val exprInfAst = NewLocal()

    Ast(exprInfAst).withChildren(annotationsAst)
  }

  def astForExprLet(filename: String, parentFullname: String, letExprInstance: ExprLet): Ast = {
    val annotationsAst = letExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val patAst         = letExprInstance.pat.toList.flatMap(pat => List(astForPat(filename, parentFullname, pat)))
    val exprAst        = letExprInstance.expr.toList.flatMap(expr => List(astForExpr(filename, parentFullname, expr)))

    val exprLetAst = NewLocal()

    Ast(exprLetAst).withChildren(annotationsAst).withChildren(patAst).withChildren(exprAst)
  }

  def astForExprLit(filename: String, parentFullname: String, litExprInstance: ExprLit): Ast = {
    val annotationsAst = litExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val exprLitAst = NewLiteral()

    Ast(exprLitAst)
      .withChildren(annotationsAst)
  }

  def astForExprLoop(filename: String, parentFullname: String, loopExprInstance: ExprLoop): Ast = {
    val annotationsAst = loopExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val labelAst = loopExprInstance.label.toList.flatMap(label => List(astForLabel(filename, parentFullname, label)))
    val bodyAst  = astForBlock(filename, parentFullname, loopExprInstance.body)
    val loopAst = NewControlStructure()
      .controlStructureType(ControlStructureTypes.DO)
      .parserTypeName(classOf[ExprLoop].getSimpleName)

    Ast(loopAst)
      .withChildren(annotationsAst)
      .withChildren(labelAst)
      .withChild(bodyAst)
  }

  def astForExprMacro(filename: String, parentFullname: String, macroExprInstance: ExprMacro): Ast = {
    val annotationsAst = macroExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val macroRustAst = Macro(macroExprInstance.path, macroExprInstance.delimiter, macroExprInstance.tokens)
    astForMacro(filename, parentFullname, macroRustAst).withChildren(annotationsAst)
  }

  def astForExprMatch(filename: String, parentFullname: String, matchExprInstance: ExprMatch): Ast = {
    val annotationsAst = matchExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val exprAst        = matchExprInstance.expr.toList.flatMap(expr => List(astForExpr(filename, parentFullname, expr)))
    val armsAst        = matchExprInstance.arms.map(astForArm(filename, parentFullname, _)).toList

    Ast()
      .withChildren(annotationsAst)
      .withChildren(exprAst)
      .withChildren(armsAst)
  }

  def astForExprMethodCall(filename: String, parentFullname: String, methodCallExprInstance: ExprMethodCall): Ast = {
    val annotationsAst =
      methodCallExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val receiverAst =
      methodCallExprInstance.receiver.toList.flatMap(receiver => List(astForExpr(filename, parentFullname, receiver)))
    val turbofishAst = methodCallExprInstance.turbofish.toList.flatMap(turbofish =>
      List(astForAngleBracketedGenericArguments(filename, parentFullname, turbofish))
    )
    val argsAst = methodCallExprInstance.args.map(astForExpr(filename, parentFullname, _)).toList

    val exprMethodCallAst = NewCall()
      .name(methodCallExprInstance.method)
      .methodFullName(methodCallExprInstance.method)

    Ast(exprMethodCallAst)
      .withChildren(annotationsAst)
      .withChildren(receiverAst)
      .withChildren(turbofishAst)
      .withChildren(argsAst)
  }

  def astForExprParen(filename: String, parentFullname: String, parenExprInstance: ExprParen): Ast = {
    val annotationsAst = parenExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))
    val exprAst        = parenExprInstance.expr.toList.flatMap(expr => List(astForExpr(filename, parentFullname, expr)))
    val exprParenAst   = NewUnknown()

    Ast(exprParenAst)
      .withChildren(annotationsAst)
      .withChildren(exprAst)
  }

  def astForExprPath(filename: String, parentFullname: String, pathExprInstance: ExprPath): Ast = {
    val annotationsAst = pathExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val path        = Path( pathExprInstance.segments, pathExprInstance.leading_colon)
    val pathAst     = astForPath(filename, parentFullname, path)
    val qselfAst    = pathExprInstance.qself.toList.flatMap(qself => List(astForQself(filename, parentFullname, qself)))
    val exprPathAst = NewTypeRef()

    Ast()
      .withChildren(annotationsAst)
      .withChild(pathAst)
      .withChildren(qselfAst)
  }

  def astForExprRange(filename: String, parentFullname: String, rangeExprInstance: ExprRange): Ast = {
    val annotationsAst = rangeExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val startAst = rangeExprInstance.start.toList.flatMap(start => List(astForExpr(filename, parentFullname, start)))
    val endAst   = rangeExprInstance.end.toList.flatMap(end => List(astForExpr(filename, parentFullname, end)))
    val limitAst =
      rangeExprInstance.limits.toList.flatMap(limit => List(astForRangeLimits(filename, parentFullname, limit)))

    val exprRangeAst = NewArrayInitializer()

    Ast(exprRangeAst)
      .withChildren(annotationsAst)
      .withChildren(startAst)
      .withChildren(limitAst)
      .withChildren(endAst)
  }

  def astForExprReference(filename: String, parentFullname: String, referenceExprInstance: ExprReference): Ast = {
    val annotationsAst = referenceExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val exprAst = referenceExprInstance.expr.toList.flatMap(expr => List(astForExpr(filename, parentFullname, expr)))
    val exprReferenceAst = NewTypeRef()

    Ast(exprReferenceAst).withChildren(annotationsAst).withChildren(exprAst)
  }

  def astForExprRepeat(filename: String, parentFullname: String, repeatExprInstance: ExprRepeat): Ast = {
    val annotationsAst = repeatExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val exprAst       = repeatExprInstance.expr.toList.flatMap(expr => List(astForExpr(filename, parentFullname, expr)))
    val lenAst        = repeatExprInstance.len.toList.flatMap(len => List(astForExpr(filename, parentFullname, len)))
    val exprRepeatAst = NewArrayInitializer()

    Ast(exprRepeatAst)
      .withChildren(annotationsAst)
      .withChildren(exprAst)
      .withChildren(lenAst)
  }

  def astForExprReturn(filename: String, parentFullname: String, returnExprInstance: ExprReturn): Ast = {
    val annotationsAst = returnExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val exprAst       = returnExprInstance.expr.toList.flatMap(expr => List(astForExpr(filename, parentFullname, expr)))
    val exprReturnAst = NewReturn()

    Ast(exprReturnAst).withChildren(annotationsAst).withChildren(exprAst)
  }

  def astForExprStruct(filename: String, parentFullname: String, structExprInstance: ExprStruct): Ast = {
    val annotationsAst = structExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val qselfAst  = structExprInstance.qself.toList.flatMap(qself => List(astForQself(filename, parentFullname, qself)))
    val pathAst   = structExprInstance.path.toList.flatMap(path => List(astForPath(filename, parentFullname, path)))
    val fieldsAst = structExprInstance.fields.map(astForFieldValue(filename, parentFullname, _)).toList
    val restAst   = structExprInstance.rest.toList.flatMap(rest => List(astForExpr(filename, parentFullname, rest)))

    val exprStructAst = NewLocal()

    Ast(exprStructAst)
      .withChildren(annotationsAst)
      .withChildren(qselfAst)
      .withChildren(pathAst)
      .withChildren(fieldsAst)
      .withChildren(restAst)
  }

  def astForExprTry(filename: String, parentFullname: String, tryExprInstance: ExprTry): Ast = {
    val annotationsAst = tryExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val exprAst = tryExprInstance.expr.toList.flatMap(expr => List(astForExpr(filename, parentFullname, expr)))
    val exprTryAst = NewControlStructure()
      .controlStructureType(ControlStructureTypes.TRY)
      .parserTypeName(classOf[ExprTry].getSimpleName)

    Ast(exprTryAst).withChildren(annotationsAst).withChildren(exprAst)
  }

  def astForExprTryBlock(filename: String, parentFullname: String, tryBlockExprInstance: ExprTryBlock): Ast = {
    val annotationsAst = tryBlockExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val stmtsAst        = astForBlock(filename, parentFullname, tryBlockExprInstance.stmts)
    val exprTryBlockAst = NewBlock()

    Ast(exprTryBlockAst)
      .withChildren(annotationsAst)
      .withChild(stmtsAst)
  }

  def astForExprTuple(filename: String, parentFullname: String, tupleExprInstance: ExprTuple): Ast = {
    val annotationsAst = tupleExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val elemsAst     = tupleExprInstance.elems.map(astForExpr(filename, parentFullname, _)).toList
    val exprTupleAst = NewArrayInitializer()

    Ast(exprTupleAst).withChildren(annotationsAst).withChildren(elemsAst)
  }

  def astForExprUnary(filename: String, parentFullname: String, unaryExprInstance: ExprUnary): Ast = {
    val annotationsAst = unaryExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val exprAst      = unaryExprInstance.expr.toList.flatMap(expr => List(astForExpr(filename, parentFullname, expr)))
    val opAst        = unaryExprInstance.op.toList.flatMap(op => List(astForUnOp(filename, parentFullname, op)))
    val exprUnaryAst = NewUnknown()

    Ast(exprUnaryAst).withChildren(annotationsAst).withChildren(exprAst).withChildren(opAst)
  }

  def astForExprUnsafe(filename: String, parentFullname: String, unsafeExprInstance: ExprUnsafe): Ast = {
    val annotationsAst = unsafeExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val stmtsAst      = astForBlock(filename, parentFullname, unsafeExprInstance.stmts)
    val exprUnsafeAst = NewBlock()

    Ast(exprUnsafeAst).withChildren(annotationsAst).withChild(stmtsAst)
  }

  def astForExprWhile(filename: String, parentFullname: String, whileExprInstance: ExprWhile): Ast = {
    val annotationsAst = whileExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val labelAst = whileExprInstance.label.toList.flatMap(label => List(astForLabel(filename, parentFullname, label)))
    val condAst  = whileExprInstance.cond.toList.flatMap(cond => List(astForExpr(filename, parentFullname, cond)))
    val bodyAst  = astForBlock(filename, parentFullname, whileExprInstance.body)
    val exprWhileAst = NewControlStructure()
      .controlStructureType(ControlStructureTypes.WHILE)
      .parserTypeName(classOf[ExprWhile].getSimpleName)

    Ast(exprWhileAst)
      .withChildren(annotationsAst)
      .withChildren(labelAst)
      .withChildren(condAst)
      .withChild(bodyAst)
  }

  def astForExprYield(filename: String, parentFullname: String, yieldExprInstance: ExprYield): Ast = {
    val annotationsAst = yieldExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))

    val exprAst      = yieldExprInstance.expr.toList.flatMap(expr => List(astForExpr(filename, parentFullname, expr)))
    val exprYieldAst = NewReturn()

    Ast(exprYieldAst).withChildren(annotationsAst).withChildren(exprAst)
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