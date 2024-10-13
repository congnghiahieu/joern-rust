package io.joern.rustsrc2cpg.parser

import com.fasterxml.jackson.databind.DeserializationFeature
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import io.joern.rustsrc2cpg.Config
import io.joern.rustsrc2cpg.Frontend.defaultConfig
import io.joern.rustsrc2cpg.ast.*
import io.joern.rustsrc2cpg.passes.AstCreationPass
import io.joern.rustsrc2cpg.passes.ModuleResolverPass
import io.joern.rustsrc2cpg.passes.TypeResolverPass
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg

object ExtendLanguages {
  final val RUSTLANG = "RUSTLANG"
}

import java.io.File
import scala.util.Try

class RustCpg extends X2CpgFrontend[Config] {

  private val report: Report = new Report()

  override def createCpg(config: Config): Try[Cpg] = {

    val inputFile = File(config.inputPath)
    if (!inputFile.isDirectory && !inputFile.isFile) {
      throw new IllegalArgumentException(s"${inputFile.toString()} is not a valid directory or file.")
    }

    X2Cpg.withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      better.files.File.usingTemporaryDirectory("rustsrc2cpg_tmp") { tempOutputDir =>
        val cargoCrate = CargoCrate(config)

        new MetaDataPass(cpg, ExtendLanguages.RUSTLANG, config.inputPath).createAndApply()

        val astCreationPass = new AstCreationPass(cpg, config, tempOutputDir.path, cargoCrate, report)
        astCreationPass.createAndApply()

      // val typeResolverPass =
      //   new TypeResolverPass(cpg, astCreationPass.getUsedPrimitiveType().toArray(Array.empty[String]))
      // typeResolverPass.createAndApply()

      // val moduleResovelerPass = new ModuleResolverPass(cpg, cargoCrate)
      // moduleResovelerPass.createAndApply()
      }
    }
  }

}
