package ar.edu.unq.tpi.qsim.integracion.mumuki

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import ar.edu.unq.tpi.qsim.model.CPU
import ar.edu.unq.tpi.qsim.model.Registro
import com.google.gson.JsonObject
import com.google.gson.JsonParser

abstract class JsonOutPut

case class JsonOk(var special_records: SpecialRecords, var flags: Flags, var records: Records) extends JsonOutPut
case class SpecialRecords(val PC: String, val SP: String, val IR: String) extends JsonOutPut
case class Flags(val N: Int, val Z: Int, val V: Int, val C: Int) extends JsonOutPut
case class Records(val R0: String, val R1: String, val R2: String, val R3: String, val R4: String,
                   val R5: String, val R6: String, val R7: String) extends JsonOutPut
case class JsonError(val error: String, val kind: String) extends JsonOutPut

class JsonResult {

  implicit def registroToString(registro: Registro): String = registro.valor.hex

  def buildJsonOk(cpu: CPU) = 
    JsonOk(
      SpecialRecords(cpu.pc.hex, cpu.sp.hex, cpu.ir),
      Flags(cpu.n, cpu.z, cpu.v, cpu.c),
      Records(cpu.registros(0), cpu.registros(1), cpu.registros(2), cpu.registros(3), cpu.registros(4),
        cpu.registros(5), cpu.registros(6), cpu.registros(7)))
        
  def parserJson(json: String) = {
    var gson = new GsonBuilder().setPrettyPrinting().create()
    var jsonObj = (new JsonParser()).parse(json).getAsJsonObject()
    generateJson(jsonObj) 
}
  
  def generateJson(jsonObj: JsonObject) : JsonOk = {
     JsonOk(
      SpecialRecords(jsonObj.get("special_records").getAsJsonObject.get("PC").getAsString(), jsonObj.get("special_records").getAsJsonObject.get("SP").getAsString(), jsonObj.get("special_records").getAsJsonObject.get("IR").getAsString()),
      Flags(jsonObj.get("flags").getAsJsonObject.get("N").getAsInt(), jsonObj.get("flags").getAsJsonObject.get("Z").getAsInt(), jsonObj.get("flags").getAsJsonObject.get("V").getAsInt(), jsonObj.get("flags").getAsJsonObject.get("C").getAsInt()),
      Records(jsonObj.get("records").getAsJsonObject.get("R0").getAsString(), jsonObj.get("records").getAsJsonObject.get("R1").getAsString(), jsonObj.get("records").getAsJsonObject.get("R2").getAsString(), jsonObj.get("records").getAsJsonObject.get("R3").getAsString(), jsonObj.get("records").getAsJsonObject.get("R4").getAsString(), jsonObj.get("records").getAsJsonObject.get("R5").getAsString(),
        jsonObj.get("records").getAsJsonObject.get("R6").getAsString(), jsonObj.get("records").getAsJsonObject.get("R7").getAsString()))
  }
}