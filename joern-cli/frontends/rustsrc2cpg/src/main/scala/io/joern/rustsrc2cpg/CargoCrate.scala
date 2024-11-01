package io.joern.rustsrc2cpg.ast

import io.joern.rustsrc2cpg.Config

import java.io.BufferedReader
import java.io.File
import java.io.FileReader
import java.nio.file.Files
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

class CargoCrate(config: Config) {

  private var _crateName: String         = ""
  private var _cratePath: String         = ""
  private var _crateIsWorkspace: Boolean = true

  loadCrate()

  private def loadCrate(): Unit = {
    val inputPath  = config.inputPath
    val projectDir = File(inputPath)
    val traverse   = ListBuffer[File]()
    traverse.addAll(projectDir.listFiles())

    var isContinue = true;
    while (isContinue) {
      val file = traverse.head

      if (file.isDirectory) {
        traverse.addAll(file.listFiles())
      } else if (file.isFile && file.getName.equals("Cargo.toml")) {
        parseGoModFile(file) match {
          case Some(name) => {
            _crateName = name
            _crateIsWorkspace = false
          }
          case None => {
            _crateName = ""
            _crateIsWorkspace = false
          }
        }
        _cratePath = file.getParentFile.getAbsolutePath
      }

      if (traverse.nonEmpty) {
        traverse.remove(0)
      }
      if (traverse.isEmpty) {
        isContinue = false
      }
    }
  }

  private def parseGoModFile(file: File): Option[String] = {
    val fileReader                = FileReader(file)
    val buffer                    = BufferedReader(fileReader)
    var crateName: Option[String] = None

    buffer
      .lines()
      .forEach(line => {
        val pattern: Regex = """name\s*=\s*"(.*?)"""".r
        val result         = pattern.findFirstMatchIn(line).map(_.group(1))

        result match {
          case Some(value) => {
            crateName = Some(value)
          }
          case None => {}
        }
      })

    crateName
  }

  def crateName: String = _crateName

  def cratePath: String = _cratePath

  def getGoVersion: String = {
    return ""
  }

  def getCrateDependencies: List[String] = {
    return List()
  }
}
