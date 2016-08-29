package ar.edu.unq.tpi.qsim.integracion.mumuki

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import ar.edu.unq.tpi.qsim.model._
import scala.collection.mutable.Map
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

abstract class JsonOutPut

case class JsonOk(var special_records: SpecialRecords, var flags: Flags, var records: Records, var memory: java.util.Map[String, java.util.Map[String, String]]) extends JsonOutPut
case class SpecialRecords(val PC: String, val SP: String, val IR: String) extends JsonOutPut
case class Flags(val N: Int, val Z: Int, val V: Int, val C: Int) extends JsonOutPut
case class Records(val R0: String, val R1: String, val R2: String, val R3: String, val R4: String,
                   val R5: String, val R6: String, val R7: String) extends JsonOutPut
case class Memory(val CELL: String) extends JsonOutPut
case class JsonError(val error: String, val kind: String) extends JsonOutPut

class JsonResult {

  implicit def registroToString(registro: Registro): String = registro.valor.hex
  var gson = new GsonBuilder().setPrettyPrinting().create()

  def buildJsonOk(sim: Simulador) =
    JsonOk(
      SpecialRecords(sim.cpu.pc.hex, sim.cpu.sp.hex, sim.cpu.ir),
      Flags(sim.cpu.n, sim.cpu.z, sim.cpu.v, sim.cpu.c),
      Records(sim.cpu.registros(0), sim.cpu.registros(1), sim.cpu.registros(2), sim.cpu.registros(3), sim.cpu.registros(4),
        sim.cpu.registros(5), sim.cpu.registros(6), sim.cpu.registros(7)),
      sim.busIO.stateMemory)

  def parserJson(json: String) = {
    gson.fromJson(json, classOf[JsonOk])
  }
}