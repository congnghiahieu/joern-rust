package io.joern.rustsrc2cpg.astcreation

import io.joern.rustsrc2cpg.ast.nodes.Identifier
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal

trait AstNodeBuilderHelper { this: AstCreator =>

  def newLocalNode(identifier: Identifier): NewLocal = {
    localNode(identifier, identifier.name.get, identifier.code, identifier.typeFullName)
  }

}
