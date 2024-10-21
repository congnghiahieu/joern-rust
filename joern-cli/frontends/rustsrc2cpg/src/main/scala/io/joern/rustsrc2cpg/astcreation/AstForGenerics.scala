package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.*
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
trait AstForGenerics(implicit schemaValidationMode: ValidationMode) { this: AstCreator =>
  def astForGenerics(filename: String, parentFullname: String, generics: Generics): Ast = {
    if (generics.params.isDefined) {
      val genericsNode = unknownNode(EmptyAst(), "")
      val genericParamsAsts = generics.params.get
        .map(astForGenericParam(filename, parentFullname, _))
        .toList
      Ast(genericsNode).withChildren(genericParamsAsts)
    } else if (generics.whereClause.isDefined) {
      val genericsNode = unknownNode(EmptyAst(), "")
      val wherePredicatesAsts = generics.whereClause.get
        .map(astForWherePredicate(filename, parentFullname, _))
        .toList
      Ast(genericsNode).withChildren(wherePredicatesAsts)

    } else {
      throw new RuntimeException(s"Unknown Generics type: $generics")
    }
  }

}
