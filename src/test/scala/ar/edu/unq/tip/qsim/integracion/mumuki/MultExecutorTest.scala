package ar.edu.unq.tip.qsim.integracion.mumuki

import ar.edu.unq.tpi.qsim.integracion.mumuki.{JsonQ, JsonError}
import ar.edu.unq.tpi.qsim.model.{Simulador, W16}
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
    val output = out.asInstanceOf[JsonQ]

    assert(output.special_records.PC.equals(contextOk.sim.cpu.pc.hex))
    assert(output.special_records.SP.equals(contextOk.sim.cpu.sp.hex))
    assert(output.special_records.IR.equals(contextOk.sim.cpu.ir))
  }

  "La Ejecucion de un Programa" should "devolver el estado de los flags cuando se ejecuta exitosamente" in {
    val (_, out) = contextQsiMain.refereeQsim.evalResult(contextOk.result)
    val output = out.asInstanceOf[JsonQ]

    assert(output.flags.C.equals(contextOk.sim.cpu.c))
    assert(output.flags.N.equals(contextOk.sim.cpu.n))
    assert(output.flags.V.equals(contextOk.sim.cpu.v))
    assert(output.flags.Z.equals(contextOk.sim.cpu.z))
  }

  "La Ejecucion de un Programa" should "devolver el estado de sus registros cuando se ejecuta exitosamente" in {
    val (_, out) = contextQsiMain.refereeQsim.evalResult(contextOk.result)
    val output = out.asInstanceOf[JsonQ]

    assert(output.records.R0.equals(contextOk.sim.cpu.registro("R0").get.getValor().hex))
    assert(output.records.R1.equals(contextOk.sim.cpu.registro("R1").get.getValor().hex))
    assert(output.records.R2.equals(contextOk.sim.cpu.registro("R2").get.getValor().hex))
    assert(output.records.R3.equals(contextOk.sim.cpu.registro("R3").get.getValor().hex))
    assert(output.records.R4.equals(contextOk.sim.cpu.registro("R4").get.getValor().hex))
    assert(output.records.R5.equals(contextOk.sim.cpu.registro("R5").get.getValor().hex))
    assert(output.records.R6.equals(contextOk.sim.cpu.registro("R6").get.getValor().hex))
    assert(output.records.R7.equals(contextOk.sim.cpu.registro("R7").get.getValor().hex))
  }

  "La Ejecucion de cualquier Programa (conteniendo o no rutinas)" should "terminar correctamente (ejecutando todas las instrucciones del programa)" in {
    val (_, out) = contextQsiMain.refereeQsim.evalResult(contextOk.result)
    val output = out.asInstanceOf[JsonQ]

    val pcFin = new W16(contextOk.qsiMain.input.special_records.PC)
    pcFin ++ contextOk.program.tamanioDelPrograma()

    assert(output.special_records.PC.equals(pcFin.hex))
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

  "El Simulador" should " devolver el estado de la memoria, compuesto por las celdas que contienen informacion, incluyendo las celdas que se cargaron con el input" in {
    val (_, out) = contextQsiMain.refereeQsim.evalResult(contextOk.result)
    val output = out.asInstanceOf[JsonQ]

    val jsonInput = contextQsiMain.jsonInput
    val outMemory = output.memory

    // todas las celdas que se cargan en el input deberian ser parte del estado de la memoria
    for (dir <- jsonInput.memory.keySet()){
      assert(outMemory.containsKey(dir))
    }
  }

  "El Simulador" should " devolver el estado la memoria, conteniendo como minimo la informacion del programa a ejecutar" in {
    val (_, out) = contextQsiMain.refereeQsim.evalResult(contextOk.result)
    val output = out.asInstanceOf[JsonQ]

    val outMemory = output.memory
    val memory = contextOk.sim.busIO.memoria

    // como minimo el tamaño del programa.
    assert(outMemory.keySet().size() >= contextOk.program.tamanioDelPrograma())
  }

  "El Simulador" should " devolver el estado la memoria, dicho estado deberia contener el valor de la celda 1000 que se cargo en el input" in {
    val (_, out) = contextQsiMain.refereeQsim.evalResult(contextOk.result)
    val output = out.asInstanceOf[JsonQ]

    val outMemory = output.memory
    val memory = contextOk.sim.busIO.memoria
    val celda = "1000"

    assert(outMemory.containsKey(celda))
    assert(outMemory(celda).equals(memory.getValor(celda).hex))
  }
}
