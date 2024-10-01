package io.joern.rustsrc2cpg

import io.joern.rustsrc2cpg.parser.RustCpg
import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
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
    rustcpg.run(config)
  }

}
