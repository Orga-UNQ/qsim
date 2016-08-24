package ar.edu.unq.tpi.qsim.integracion.mumuki

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import ar.edu.unq.tpi.qsim.model._
import com.google.gson.JsonObject
import com.google.gson.JsonParser
import scala.collection.mutable.Map
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

abstract class JsonOutPut

case class JsonOk(var special_records: SpecialRecords, var flags: Flags, var records: Records, var memory: Memory) extends JsonOutPut
case class SpecialRecords(val PC: String, val SP: String, val IR: String) extends JsonOutPut
case class Flags(val N: Int, val Z: Int, val V: Int, val C: Int) extends JsonOutPut
case class Records(val R0: String, val R1: String, val R2: String, val R3: String, val R4: String,
                   val R5: String, val R6: String, val R7: String) extends JsonOutPut
case class Memory(val CELL: String) extends JsonOutPut
case class JsonError(val error: String, val kind: String) extends JsonOutPut

class JsonResult {

  implicit def registroToString(registro: Registro): String = registro.valor.hex
  var gson = new GsonBuilder().setPrettyPrinting().create()
  //Json(DefaultFormats).write(sim.busIO.stateMemory)

  def buildJsonOk(sim: Simulador) =
    JsonOk(
      SpecialRecords(sim.cpu.pc.hex, sim.cpu.sp.hex, sim.cpu.ir),
      Flags(sim.cpu.n, sim.cpu.z, sim.cpu.v, sim.cpu.c),
      Records(sim.cpu.registros(0), sim.cpu.registros(1), sim.cpu.registros(2), sim.cpu.registros(3), sim.cpu.registros(4),
        sim.cpu.registros(5), sim.cpu.registros(6), sim.cpu.registros(7)),
      Memory(pretty(sim.busIO.stateMemory)))

  def parserJson(json: String) = {
    var jsonObj = (new JsonParser()).parse(json).getAsJsonObject()
    Console.print("Que es jsonObj \n")
    Console.print(jsonObj + "\n")
    generateJson(jsonObj)
  }

  def generateJson(jsonObj: JsonObject): JsonOk = {
    JsonOk(
      SpecialRecords(jsonObj.get("special_records").getAsJsonObject.get("PC").getAsString(), jsonObj.get("special_records").getAsJsonObject.get("SP").getAsString(), jsonObj.get("special_records").getAsJsonObject.get("IR").getAsString()),
      Flags(jsonObj.get("flags").getAsJsonObject.get("N").getAsInt(), jsonObj.get("flags").getAsJsonObject.get("Z").getAsInt(), jsonObj.get("flags").getAsJsonObject.get("V").getAsInt(), jsonObj.get("flags").getAsJsonObject.get("C").getAsInt()),
      Records(jsonObj.get("records").getAsJsonObject.get("R0").getAsString(), jsonObj.get("records").getAsJsonObject.get("R1").getAsString(), jsonObj.get("records").getAsJsonObject.get("R2").getAsString(), jsonObj.get("records").getAsJsonObject.get("R3").getAsString(), jsonObj.get("records").getAsJsonObject.get("R4").getAsString(), jsonObj.get("records").getAsJsonObject.get("R5").getAsString(),
        jsonObj.get("records").getAsJsonObject.get("R6").getAsString(), jsonObj.get("records").getAsJsonObject.get("R7").getAsString()),
      Memory(jsonObj.get("memory").getAsJsonObject.get("CELL").toString()))
  }
}