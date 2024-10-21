package io.joern.rustsrc2cpg

import better.files.File
import io.joern.rustsrc2cpg.parser.RustCpg
import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.X2CpgMain
import scopt.OParser

final case class Config() extends X2CpgConfig[Config]

private object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.programName
    OParser.sequence(programName("rustsrc2cpg"))
  }
}

object Main extends X2CpgMain(Frontend.cmdLineParser, new RustCpg())(Frontend.defaultConfig) {
  def run(config: Config, rustcpg: RustCpg): Unit = {
    val inputDir = File(config.inputPath)
    if (inputDir.isDirectory) {
      if (inputDir.name.endsWith("projects")) {
        inputDir.list.filter(_.isDirectory).foreach { directory =>
          config.inputPath = directory.pathAsString
          var testRustCpg = RustCpg()
          testRustCpg.run(config)
        }
      } else {
        rustcpg.run(config)
      }
    } else if (inputDir.isRegularFile) {
      rustcpg.run(config)
    }
  }

}
