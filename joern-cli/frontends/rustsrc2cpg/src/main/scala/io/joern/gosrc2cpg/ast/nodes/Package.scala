package io.joern.rustsrc2cpg.ast.nodes

class Package extends Node {
  var name: String                 = ""
  var files: Map[String, FileNode] = Map()
}
