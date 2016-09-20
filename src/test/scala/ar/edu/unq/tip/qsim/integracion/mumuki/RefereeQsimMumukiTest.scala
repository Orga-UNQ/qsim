package ar.edu.unq.tip.qsim.integracion.mumuki

/**
  * Copyright 2014 Tatiana Molinari.
  * Copyright 2014 Susana Rosito
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program. If not, see <http://www.gnu.org/licenses/>.
  *
  */

import ar.edu.unq.tpi.qsim.exeptions.{RuntimeErrorException, SyntaxErrorException}
import ar.edu.unq.tpi.qsim.integracion.mumuki._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ar.edu.unq.tpi.qsim.model._
import org.uqbar.commons.model.UserException
import scala.collection.mutable
import scala.collection.mutable._
import ar.edu.unq.tpi.qsim.utils._
import scala.collection.JavaConversions._

class RefereeQsimMumukiTest extends FlatSpec with Matchers {

  def programtets = new {

    var inst1 = MOV(R3, new Inmediato("0003"))
    var inst2 = MOV(R5, new Inmediato("0004"))
    var inst3 = ADD(R3, R5)
    var inst4 = MOV(new Inmediato("0002"), new Inmediato("0003"))
    var inst5 = CALL(new Etiqueta("sumarDos"))

    var programaOk = new Programa(List(inst1, inst2, inst3))
    var programaExe = new Programa(List(inst4, inst2, inst3))
    var programaCom = new Programa(List(inst5, inst2, inst3))
  }

  def contextqsiMain = new {

    val arqQ = 0
    // ArquitecturaQ1
    val multExe = new MultExecutor()
    var refereeQsim = new RefereeQsimMumuki()

    val specialRecords = SpecialRecords("0000", "FFEF", "0000")
    val flags = Flags(0, 0, 0, 0)
    val records = Records("0000", "0000", "0000", "0000", "0000", "0000", "0000", "0000")
    val memory = Map[String, String]("1000" -> "0002", "1001" -> "0003", "1002" -> "0004", "1003" -> "0005")
    val jsonInput = JsonInputOk(1, specialRecords, flags, records, memory)
  }

  def contextOk = new {

    val programa = programtets.programaOk
    val qsiMain = new QsimMainMumuki()
    val sim = Simulador()

    qsiMain.program = programa
    qsiMain.setInput(contextqsiMain.jsonInput)
    qsiMain.selectArqQ(contextqsiMain.arqQ)
    qsiMain.loadValuesOfInput()

    val result = contextqsiMain.multExe.runQsim(sim, qsiMain)
  }

  def contextExe = new {

    var programa = programtets.programaExe
    val qsiMain = new QsimMainMumuki()
    val sim = Simulador()

    qsiMain.program = programa
    qsiMain.setInput(contextqsiMain.jsonInput)
    qsiMain.selectArqQ(contextqsiMain.arqQ)
    qsiMain.loadValuesOfInput()

    val result =
      try {
        contextqsiMain.multExe.runQsim(sim, qsiMain)
      } catch {
        case ex: UserException => JsonError(ex.getMessage, "runtime")
      }
  }

  def contextLoadInput = new {

    var programa = programtets.programaOk
    val qsiMain = new QsimMainMumuki()
    val sim = Simulador()
    val arqQ = 0

    qsiMain.program = programa
    qsiMain.setInput(contextqsiMain.jsonInput)
    qsiMain.selectArqQ(arqQ)
    qsiMain.loadValuesOfInput()

    val result =
      try {
        sim.inicializarSim(qsiMain.flags, qsiMain.positionMemoryInput)
      } catch {
        case ex: UserException => JsonError(ex.getMessage, "runtime")
      }
  }

  def contextCom = new {

    var programa = programtets.programaCom
    val qsiMain = new QsimMainMumuki()
    val sim = Simulador()
    val arqQ = 5

    qsiMain.program = programa
    qsiMain.setInput(contextqsiMain.jsonInput)
    qsiMain.selectArqQ(arqQ)
    qsiMain.loadValuesOfInput()

    val result =
      try {
        contextqsiMain.multExe.runQsim(sim, qsiMain)
      } catch {
        case ex: UserException => JsonError(ex.getMessage, "runtime")
      }
  }

  //------------------------------------------------------Caso exitoso------------

  "Un Programa" should "ejecutar exitosamente si esta bien escrito y la arquitectura elegida es la correcta" in {
    val (code, output) = contextqsiMain.refereeQsim.evalResult(contextOk.result)
    assert(code.equals(0))
  }

  "La Ejecucion de un Programa" should "devolver el estado de los registros especiales cuando se ejecuta exitosamente" in {
    val (code, output) = contextqsiMain.refereeQsim.evalResult(contextOk.result)
    val jsonOutOk = output.asInstanceOf[JsonOutOk]

    assert(jsonOutOk.special_records.PC.equals(contextOk.sim.cpu.pc.hex))
    assert(jsonOutOk.special_records.SP.equals(contextOk.sim.cpu.sp.hex))
    assert(jsonOutOk.special_records.IR.equals(contextOk.sim.cpu.ir))
  }

  "La Ejecucion de un Programa" should "devolver el estado de los flags cuando se ejecuta exitosamente" in {
    val (code, output) = contextqsiMain.refereeQsim.evalResult(contextOk.result)
    val jsonOutOk = output.asInstanceOf[JsonOutOk]

    assert(jsonOutOk.flags.C.equals(contextOk.sim.cpu.c))
    assert(jsonOutOk.flags.N.equals(contextOk.sim.cpu.n))
    assert(jsonOutOk.flags.V.equals(contextOk.sim.cpu.v))
    assert(jsonOutOk.flags.Z.equals(contextOk.sim.cpu.z))
  }

  "La Ejecucion de un Programa" should "devolver el estado de sus registros cuando se ejecuta exitosamente" in {
    val (code, output) = contextqsiMain.refereeQsim.evalResult(contextOk.result)
    val jsonOutOk = output.asInstanceOf[JsonOutOk]

    assert(jsonOutOk.records.R0.equals(contextOk.sim.cpu.registro("R0").get.getValor().hex))
    assert(jsonOutOk.records.R1.equals(contextOk.sim.cpu.registro("R1").get.getValor().hex))
    assert(jsonOutOk.records.R2.equals(contextOk.sim.cpu.registro("R2").get.getValor().hex))
    assert(jsonOutOk.records.R3.equals(contextOk.sim.cpu.registro("R3").get.getValor().hex))
    assert(jsonOutOk.records.R4.equals(contextOk.sim.cpu.registro("R4").get.getValor().hex))
    assert(jsonOutOk.records.R5.equals(contextOk.sim.cpu.registro("R5").get.getValor().hex))
    assert(jsonOutOk.records.R6.equals(contextOk.sim.cpu.registro("R6").get.getValor().hex))
    assert(jsonOutOk.records.R7.equals(contextOk.sim.cpu.registro("R7").get.getValor().hex))
  }
  // ---------------------------------------------------Caso Erroneo ---------

  "Un Programa" should " ejecutar incorrectamente cuando ocurre un error en su ejecucion: Caso  => Modo Inmediato en Destino" in {
    val (code, output) = contextqsiMain.refereeQsim.evalResult(contextExe.result)
    val jsonOutError = output.asInstanceOf[JsonError]

    assert(code.equals(-1))
    assert(jsonOutError.error.equals("Un Inmediato no puede ser un operando destino."))
  }

  // ---------------------------------------------------Caso Compilacion---------

  "Un Programa" should " ejecutar incorrectamente cuando ocurre un error de compilacion : Caso => llamar a una rutina que no existe" in {
    val (code, output) = contextqsiMain.refereeQsim.evalResult(contextCom.result)
    val jsonOutError = output.asInstanceOf[JsonError]

    assert(code.equals(-1))
    assert(jsonOutError.error.equals("Una de las etiquetas utilizadas es invalida"))
  }

  "El Simulador" should " cargar correctamente el estado inicial de cada celda de memoria que se indica en el archivo input" in {
    val ctx = contextLoadInput
    val ctxqsim = ctx.qsiMain

    for (celda <- ctxqsim.positionMemoryInput.keySet()) {
      val valueInput = ctxqsim.positionMemoryInput.get(celda)
      val valueMem = ctx.sim.busIO.memoria.getValor(celda).hex
      assert(valueInput.equals(valueMem))
    }
  }

  "El Simulador" should " devolver el estado total de la memoria, por ende el output tiene que tener el tam memoria / 16" in {
    val (code, output) = contextqsiMain.refereeQsim.evalResult(contextOk.result)

    val jsonInput = contextqsiMain.jsonInput
    val outMemory = output.asInstanceOf[JsonOutOk].memory
    val memory = contextOk.sim.busIO.memoria
    val countSet = memory.tamanioMemoria() / 16

    assert(outMemory.keySet().size().equals(countSet))
  }

  "El Simulador" should " devolver el estado total de la memoria, validando el valor de la celda 1001" in {
    val (code, output) = contextqsiMain.refereeQsim.evalResult(contextOk.result)

    val outMemory = output.asInstanceOf[JsonOutOk].memory
    val memory = contextOk.sim.busIO.memoria
    val valueOut = outMemory.get("1000").get(1)

    assert(valueOut.equals(memory.getValor("1001").hex))
  }
}