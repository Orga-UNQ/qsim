package ar.edu.unq.tip.qsim.integracion.mumuki

import java.util
import ar.edu.unq.tpi.qsim.exeptions.{RuntimeErrorException, SyntaxErrorException}
import ar.edu.unq.tpi.qsim.integracion.mumuki.{JsonError, JsonOutPut, JsonInputOk}
import ar.edu.unq.tpi.qsim.model.Simulador
import com.google.gson.GsonBuilder
import org.uqbar.commons.model.UserException
import scala.collection.JavaConversions._

/**
  * Created by susy on 15/09/16.
  */
class MultExecutor {

  var sim : Simulador = _
  var qsiMain = new QsimMainMumuki()
  var refereeQsim = new RefereeQsimMumuki()
  var gson = new GsonBuilder().setPrettyPrinting().create()
  var result: JsonOutPut = _

  def exeInputs(program: String, arqQ: Integer, input: String): Unit ={
    val inputs: java.util.List[JsonInputOk] = qsiMain.inputParser(input)
    var outputs: java.util.List[JsonOutPut] = new util.LinkedList[JsonOutPut]()

    for(in <- inputs){
      exeProgramWithInput(program, arqQ, in)
      val (code, output) = refereeQsim.evalResult(result)
      outputs.add(output)

    }
    Console.println(gson.toJson(outputs))
  }

  def exeProgramWithInput(program: String, arqQ: Integer, in: JsonInputOk): Unit ={
    sim = new Simulador()
    result =
      try {
        qsiMain.setPathFile(program)
        qsiMain.selectArqQ(arqQ)
        qsiMain.agregarInput(in)
        qsiMain.ensamblar()
        sim.inicializarSim(qsiMain.flags, qsiMain.positionMemoryInput)
        sim.cargarProgramaYRegistros(qsiMain.program, qsiMain.input.special_records.PC, qsiMain.registerInput)
        sim.execute_all_program()
      } catch {
        case ex: SyntaxErrorException  => JsonError(ex.getMessage, "syntax")
        case ex: RuntimeErrorException => JsonError(ex.getMessage, "runtime")
        case ex: UserException         => JsonError(ex.getMessage, "runtime")
        case ex: Throwable             => JsonError(ex.getMessage, "unknown")
      }
    qsiMain.cleanState()
  }
}