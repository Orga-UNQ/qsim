package ar.edu.unq.tip.qsim.integracion.mumuki

import scala.collection.JavaConversions._
import ar.edu.unq.tpi.qsim.model.Programa
import ar.edu.unq.tpi.qsim.parser.ArquitecturaQ
import ar.edu.unq.tpi.qsim.parser.Parser
import ar.edu.unq.tpi.qsim.integracion.mumuki.JsonResult
import ar.edu.unq.tpi.qsim.model.W16
import scala.collection.mutable.Map
import ar.edu.unq.tpi.qsim.integracion.mumuki.JsonQ

class QsimMainMumuki {

  var files: java.util.List[Archivo] = scala.collection.immutable.List[Archivo]()
  var current: Archivo = _
  var arqCurrent: ArquitecturaQ = _
  var program: Programa = _
  var programCounter = "0000"
  var input: JsonQ = _
  var registerInput: Map[String, W16] = _
  var flags: Map[String, Any] = _
  var positionMemoryInput: java.util.Map[String, String] = Map[String, String]()

  def setPathFile(path: String) {
    if (path != null) {
      var nombre = takeName(path)
      var codigo = readFile(path)
      var archivo = new Archivo(nombre, codigo)
      files = files.+:(archivo)
    }
  }

  def cleanState(): Unit ={
    files.clear()
  }
  def getPathfile() = ""

  def readFile(path: String) = {
    val input = io.Source.fromFile(path)
    input.mkString
  }

  def ensamblar() {
    program = null
    program = arqCurrent.parser(files.map(_.codigo).mkString)
  }

  def selectArqQ(arqQ: Integer) {
    arqCurrent = Parser.arquitecturas(arqQ)
  }

  def takeName(path: String) = {
    var part_path = path.split("/")
    part_path(part_path.length - 1)
  }

  def inputParser(path: String) = {
    var inFile = readFile(path)
    new JsonResult().parserJson(inFile)
  }

  def setInput(inputOk: JsonQ): Unit ={
    input = inputOk
  }
   
  def loadValuesOfInput() {
    registerInput = Map[String, W16]("R0" -> new W16(input.records.R0), "R1" -> new W16(input.records.R1), "R2" -> new W16(input.records.R2), "R3" -> new W16(input.records.R3), "R4" -> new W16(input.records.R4), "R5" -> new W16(input.records.R5), "R6" -> new W16(input.records.R6), "R7" -> new W16(input.records.R7))
    flags = Map[String, Any]("v" -> input.flags.V, "c" -> input.flags.C, "z" -> input.flags.Z, "n" -> input.flags.N)
    positionMemoryInput = input.memory
  }
}