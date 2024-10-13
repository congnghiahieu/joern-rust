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

import java.nio.file.Paths
import java.util
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class AstCreator(
  rootNode: FileAst,
  filename: String,
  cargoCrate: CargoCrate,
  protected var usedPrimitiveTypes: util.Set[String]
)(implicit val validationMode: ValidationMode)
    extends AstCreatorBase(filename)
    with AstForAbi
    with AstForArm
    with AstForAttribute
    with AstForExpr
    with CodeForExpr
    with AstForFields
    with AstForFn
    with AstForFnArg
    with AstForForeignItem
    with AstForGenericArgument
    with CodeForGenericArgument
    with AstForGenericParam
    with AstForGenerics
    with AstForImplItem
    with AstForItem
    with AstForMacro
    with AstForMember
    with AstForPat
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
      .name("\\")
      .fullName(cargoCrate.moduleName)
      .filename("")
    val namespaceAst = Ast(namespaceBlock)

    namespaceStack.push(namespaceAst.root.get)

    val parentPath = Paths.get(cargoCrate.modulePath);
    val childPath  = Paths.get("")
    val filePath   = parentPath.relativize(childPath).toString
    scope.pushNewScope(namespaceBlock)

    val annotationsAst = root.attrs.toList.flatMap(_.map(astForAttribute(filePath, parentFullname, _)))
    val itemAst        = root.items.map(astForItem(filePath, parentFullname, _)).toList

    scope.popScope()
    namespaceStack.pop()

    namespaceAst
  }

  //  TODO: Need implements correctly
  protected override def code(node: RustAst): String = ""

  protected override def column(node: RustAst): Option[Integer] = Option(0)

  protected override def columnEnd(node: RustAst): Option[Integer] = Option(0)

  protected override def line(node: RustAst): Option[Integer] = Option(0)

  protected override def lineEnd(node: RustAst): Option[Integer] = Option(0)
}
