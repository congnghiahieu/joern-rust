package io.joern.rustsrc2cpg.passes

import io.joern.rustsrc2cpg.ast.CargoCrate
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import overflowdb.BatchedUpdate

class ModuleResolverPass(cpg: Cpg, cargoCrate: CargoCrate) extends CpgPass(cpg) {

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val moduleNamespace = NewNamespaceBlock()
      .name(cargoCrate.crateName)
      .filename("\\")
      .fullName(cargoCrate.crateName)
    builder.addNode(moduleNamespace)
    cpg.namespaceBlock.foreach(namespaceBlock => {
      if (!namespaceBlock.name.equals("<global>")) {
        builder.addEdge(moduleNamespace, namespaceBlock, EdgeTypes.AST)
      }
    })
  }
}
