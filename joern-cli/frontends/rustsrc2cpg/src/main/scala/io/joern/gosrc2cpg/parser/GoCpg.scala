package io.joern.rustsrc2cpg.parser

import io.joern.rustsrc2cpg.Config
import io.joern.rustsrc2cpg.Frontend.defaultConfig
import io.joern.rustsrc2cpg.ast.GoModule
import io.joern.rustsrc2cpg.passes.{AstCreationPass, ModuleResolverPass, TypeResolverPass}
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.utils.Report
import io.joern.x2cpg.{X2Cpg, X2CpgFrontend}
import io.shiftleft.codepropertygraph.Cpg

object ExtendLanguages {
  final val RUSTLANG = "RUSTLANG"
}

import java.io.File
import scala.util.Try

class RustCpg extends X2CpgFrontend[Config] {

  private val report: Report = new Report()

  override def createCpg(config: Config): Try[Cpg] = {
    val inputFile = new File(config.inputPath)
    if (!inputFile.isDirectory && !inputFile.isFile) {
      throw new IllegalArgumentException(s"$inputFile is not a valid directory or file.")
    }

    X2Cpg.withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      val goModule = GoModule(config)
      better.files.File.usingTemporaryDirectory("rustsrc2cpg_tmp") { tempWorkingDir =>
        new MetaDataPass(cpg, ExtendLanguages.RUSTLANG, config.inputPath).createAndApply()
        val astCreationPass = new AstCreationPass(cpg, config, tempWorkingDir.pathAsString, goModule, report)
        astCreationPass.createAndApply()
        val typeResolverPass =
          new TypeResolverPass(cpg, astCreationPass.getUsedPrimitiveType().toArray(Array.empty[String]))
        typeResolverPass.createAndApply()
        new ModuleResolverPass(cpg, goModule).createAndApply()
      }
    }
  }

}
