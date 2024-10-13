package io.joern.rustsrc2cpg.ast

import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonValue
import io.joern.rustsrc2cpg.ast.*

enum StaticMutability(val value: String) extends RustAst {
  case Mut  extends StaticMutability("mut")
  case None extends StaticMutability("")

  @JsonValue
  override def toString: String = value
}

object StaticMutability {
  @JsonCreator
  def fromString(value: String): StaticMutability = value match {
    case "mut" => StaticMutability.Mut
    case ""    => StaticMutability.None
    case _     => throw new IllegalArgumentException(s"Unknown value: $value")
  }
}
