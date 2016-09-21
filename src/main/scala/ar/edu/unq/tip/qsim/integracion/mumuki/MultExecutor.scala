package ar.edu.unq.tip.qsim.integracion.mumuki

import java.util
import ar.edu.unq.tpi.qsim.exeptions.{RuntimeErrorException, SyntaxErrorException}
import ar.edu.unq.tpi.qsim.integracion.mumuki.{JsonError, JsonOutPut, JsonInputOk}
import ar.edu.unq.tpi.qsim.model.Simulador
import org.uqbar.commons.model.UserException
import scala.collection.JavaConversions._

/**
  * Created by susy on 15/09/16.
  */
class MultExecutor {

  val qsiMain = new QsimMainMumuki()

  def exeInputs(program: String, arqQ: Integer, input: String)={
    val refereeQsim = new RefereeQsimMumuki()
    val inputs: java.util.List[JsonInputOk] = qsiMain.inputParser(input)
    val outputs: java.util.List[JsonOutPut] = new util.LinkedList[JsonOutPut]()

    for(in <- inputs) {
      val result = exeProgramWithInput(program, arqQ, in)
      val (code, output) = refereeQsim.evalResult(result)
      outputs.add(output)
    }
    outputs
  }

  def exeProgramWithInput(program: String, arqQ: Integer, input: JsonInputOk)={
    val sim = new Simulador()
    sim.setInputExeActual(input.id)
    val result : JsonOutPut =
      try {
        qsiMain.setPathFile(program)
        qsiMain.selectArqQ(arqQ)
        qsiMain.setInput(input)
        qsiMain.loadValuesOfInput()
        qsiMain.ensamblar()
        runQsim(sim, qsiMain)
      } catch {
        case ex: SyntaxErrorException  => JsonError(input.id, ex.getMessage, "syntax")
        case ex: RuntimeErrorException => JsonError(input.id, ex.getMessage, "runtime")
        case ex: UserException         => JsonError(input.id, ex.getMessage, "runtime")
        case ex: Throwable             => JsonError(input.id, ex.getMessage, "unknown")
      }
    qsiMain.cleanState()
    result
  }

  def runQsim(sim: Simulador, qsiMain: QsimMainMumuki)={

    sim.inicializarSim(qsiMain.flags, qsiMain.positionMemoryInput)
    sim.cargarProgramaYRegistros(qsiMain.program, qsiMain.input.special_records.PC, qsiMain.registerInput)
    sim.execute_all_program()
  }
}