package io.joern.rustsrc2cpg.ast

import io.joern.rustsrc2cpg.ast.*

import scala.collection.mutable.ListBuffer

type Label = String

class Arm extends RustAst {
  var attrs: Option[List[Attribute]] = None
  var pat: Option[Pat]               = None
  var guard: Option[Expr]            = None
  var body: Option[Expr]             = None
}
