package io.joern.rustsrc2cpg.astcreation

import com.fasterxml.jackson.databind.ObjectMapper
import io.joern.rustsrc2cpg.ast.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Scope
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.util
import scala.collection.mutable.ListBuffer

class AstCreator(
  rootNode: FileAst,
  relFilepath: String,
  cargoCrate: CargoCrate,
  protected var usedPrimitiveTypes: util.Set[String]
)(implicit val validationMode: ValidationMode)
    extends AstCreatorBase(relFilepath)
    with AstForAbi
    with AstForArm
    with AstForAttribute
    with AstForExpr
    with CodeForExpr
    with AstForFields
    with CodeForFields
    with AstForFn
    with AstForFnArg
    with AstForForeignItem
    with AstForGenericArgument
    with CodeForGenericArgument
    with AstForGenericParam
    with AstForGenerics
    with AstForImplItem
    with AstForItem
    with AstForLit
    with AstForMacro
    with AstForMember
    with CodeForMember
    with AstForPat
    with CodeForPat
    with AstForOps
    with AstForMeta
    with CodeForMeta
    with AstForPathArguments
    with CodeForPathArguments
    with AstForRangeLimits
    with AstForStmt
    with AstForTokenTree
    with CodeForTokenTree
    with AstForTraitBoundModifier
    with AstForTraitItem
    with AstForType
    with AstForTypeParamBound
    with CodeForTypeParamBound
    with TypeFullnameForType
    with AstForUseTree
    with CodeForUseTree
    with AstForVisibility
    with AstForWherePredicate
    with AstNodeBuilder[RustAst, AstCreator] {

  protected val logger: Logger                                    = LoggerFactory.getLogger(classOf[AstCreator])
  protected val objectMapper: ObjectMapper                        = ObjectMapper()
  protected val namespaceStack: util.Stack[NewNode]               = new util.Stack()
  protected val namespaceMap: util.Map[String, NewNamespaceBlock] = new util.HashMap()
  protected val typeSet: util.Set[String]                         = new util.HashSet[String]()
  protected var globalAst: Option[Ast]                            = None
  protected val scope: Scope[String, (NewNode, String), NewNode]  = new Scope()

  def createAst(): DiffGraphBuilder = {
    val ast = astForTranslationUnit(rootNode)
    Ast.storeInDiffGraph(ast, diffGraph)
    globalAst = Option(ast)
    diffGraph
  }

  private def astForTranslationUnit(root: FileAst): Ast = {
    val parentFullname = ""
    val namespaceBlock = NewNamespaceBlock()
      .name(cargoCrate.crateName)
      .fullName(cargoCrate.crateName)
      .filename(relFilepath)
    val namespaceAst = Ast(namespaceBlock)

    namespaceStack.push(namespaceAst.root.get)
    scope.pushNewScope(namespaceBlock)

    val annotationsAst = root.attrs match {
      case Some(attrs) => attrs.map(astForAttribute(relFilepath, parentFullname, _)).toList
      case None        => List()
    }
    val itemAst = root.items.map(astForItem(relFilepath, parentFullname, _)).toList

    scope.popScope()
    namespaceStack.pop()

    namespaceAst
      .withChildren(annotationsAst)
      .withChildren(itemAst)
  }

  //  TODO: Need implements correctly
  protected override def code(node: RustAst): String = ""

  protected override def column(node: RustAst): Option[Integer] = Option(0)

  protected override def columnEnd(node: RustAst): Option[Integer] = Option(0)

  protected override def line(node: RustAst): Option[Integer] = Option(0)

  protected override def lineEnd(node: RustAst): Option[Integer] = Option(0)
}
