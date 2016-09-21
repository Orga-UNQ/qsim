package ar.edu.unq.tip.qsim.integracion.mumuki

import ar.edu.unq.tpi.qsim.integracion.mumuki.{JsonError, JsonOutOk}
import ar.edu.unq.tpi.qsim.model.Simulador
import org.uqbar.commons.model.UserException
import scala.collection.JavaConversions._

/**
  * Created by susy on 21/09/16.
  */
class MultExecutorTest  extends RefereeQsimMumukiTest {


  def contextInput = new {

    val program = programs.programaOk
    var qsiMain = new QsimMainMumuki()
    var sim = Simulador()

    qsiMain.program = program
    qsiMain.setInput(contextQsiMain.jsonInput)
    qsiMain.selectArqQ(contextQsiMain.arqQ)
    qsiMain.loadValuesOfInput()
    sim.setInputExeActual(qsiMain.input.id)

    val result =
      try {
        sim.inicializarSim(qsiMain.flags, qsiMain.positionMemoryInput)
      } catch {
        case ex: UserException => JsonError(qsiMain.input.id, ex.getMessage, "runtime")
      }
  }


  "La Ejecucion de un Programa" should "devolver el estado de los registros especiales cuando se ejecuta exitosamente" in {
    val (_,out)  = contextQsiMain.refereeQsim.evalResult(contextOk.result)
    val output = out.asInstanceOf[JsonOutOk]

    assert(output.special_records.PC.equals(contextOk.sim.cpu.pc.hex))
    assert(output.special_records.SP.equals(contextOk.sim.cpu.sp.hex))
    assert(output.special_records.IR.equals(contextOk.sim.cpu.ir))
  }

  "La Ejecucion de un Programa" should "devolver el estado de los flags cuando se ejecuta exitosamente" in {
    val (_, out) = contextQsiMain.refereeQsim.evalResult(contextOk.result)
    val output = out.asInstanceOf[JsonOutOk]

    assert(output.flags.C.equals(contextOk.sim.cpu.c))
    assert(output.flags.N.equals(contextOk.sim.cpu.n))
    assert(output.flags.V.equals(contextOk.sim.cpu.v))
    assert(output.flags.Z.equals(contextOk.sim.cpu.z))
  }

  "La Ejecucion de un Programa" should "devolver el estado de sus registros cuando se ejecuta exitosamente" in {
    val (_, out) = contextQsiMain.refereeQsim.evalResult(contextOk.result)
    val output = out.asInstanceOf[JsonOutOk]

    assert(output.records.R0.equals(contextOk.sim.cpu.registro("R0").get.getValor().hex))
    assert(output.records.R1.equals(contextOk.sim.cpu.registro("R1").get.getValor().hex))
    assert(output.records.R2.equals(contextOk.sim.cpu.registro("R2").get.getValor().hex))
    assert(output.records.R3.equals(contextOk.sim.cpu.registro("R3").get.getValor().hex))
    assert(output.records.R4.equals(contextOk.sim.cpu.registro("R4").get.getValor().hex))
    assert(output.records.R5.equals(contextOk.sim.cpu.registro("R5").get.getValor().hex))
    assert(output.records.R6.equals(contextOk.sim.cpu.registro("R6").get.getValor().hex))
    assert(output.records.R7.equals(contextOk.sim.cpu.registro("R7").get.getValor().hex))
  }


  "El Simulador" should " cargar correctamente el estado inicial de cada celda de memoria que se indica en el archivo input" in {
    val ctx = contextInput
    val ctxqsim = ctx.qsiMain

    for (celda <- ctxqsim.positionMemoryInput.keySet()) {
      val valueInput = ctxqsim.positionMemoryInput.get(celda)
      val valueMem = ctx.sim.busIO.memoria.getValor(celda).hex
      assert(valueInput.equals(valueMem))
    }
  }

  "El Simulador" should " devolver el estado total de la memoria, por ende el output tiene que tener el tam memoria / 16" in {
    val (_, out) = contextQsiMain.refereeQsim.evalResult(contextOk.result)
    val output = out.asInstanceOf[JsonOutOk]

    val jsonInput = contextQsiMain.jsonInput
    val outMemory = output.memory
    val memory = contextOk.sim.busIO.memoria
    val countSet = memory.tamanioMemoria() / 16

    assert(jsonInput.id.equals(output.id))
    assert(outMemory.keySet().size().equals(countSet))
  }

  "El Simulador" should " devolver el estado total de la memoria, validando el valor de la celda 1001" in {
    val (_, out) = contextQsiMain.refereeQsim.evalResult(contextOk.result)
    val output = out.asInstanceOf[JsonOutOk]

    val outMemory = output.memory
    val memory = contextOk.sim.busIO.memoria
    val valueOut = outMemory.get("1000").get(1)

    assert(valueOut.equals(memory.getValor("1001").hex))
  }
}
