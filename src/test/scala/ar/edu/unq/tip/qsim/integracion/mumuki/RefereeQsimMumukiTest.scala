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

import ar.edu.unq.tpi.qsim.integracion.mumuki._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ar.edu.unq.tpi.qsim.model._
import org.uqbar.commons.model.UserException
import scala.collection.mutable._
import ar.edu.unq.tpi.qsim.utils._
import scala.collection.JavaConversions._

class RefereeQsimMumukiTest extends FlatSpec with Matchers {

  def programs = new {

    var inst1 = MOV(R3, new Inmediato("0003"))
    var inst2 = MOV(R5, new Inmediato("0004"))
    var inst3 = ADD(R3, R5)
    var inst4 = MOV(new Inmediato("0002"), new Inmediato("0003"))
    var inst5 = CALL(new Etiqueta("sumarDos"))

    var programaOk = new Programa(List(inst1, inst2, inst3))
    var programaExe = new Programa(List(inst4, inst2, inst3))
    var programaCom = new Programa(List(inst5, inst2, inst3))
  }

  def contextQsiMain = new {

    val arqQ = 0
    val multExe = new MultExecutor()
    var refereeQsim = new RefereeQsimMumuki()

    val specialRecords = SpecialRecords("0000", "FFEF", "0000")
    val flags = Flags(0, 0, 0, 0)
    val records = Records("0000", "0000", "0000", "0000", "0000", "0000", "0000", "0000")
    val memory = Map[String, String]("1000" -> "0002", "1001" -> "0003", "1002" -> "0004", "1003" -> "0005")
    val jsonInput = JsonInputOk(1, specialRecords, flags, records, memory)
  }

  def contextOk = new {

    val program = programs.programaOk
    var qsiMain = new QsimMainMumuki()
    var sim = Simulador()

    qsiMain.program = program
    qsiMain.setInput(contextQsiMain.jsonInput)
    qsiMain.selectArqQ(contextQsiMain.arqQ)
    qsiMain.loadValuesOfInput()

    sim.setInputExeActual(qsiMain.input.id)

    val result = contextQsiMain.multExe.runQsim(sim, qsiMain)
  }

  def contextExe = new {

    val program = programs.programaExe
    var qsiMain = contextOk.qsiMain
    var sim = contextOk.sim

    qsiMain.program = program
    val result =
      try {
        contextQsiMain.multExe.runQsim(sim, qsiMain)
      } catch {
        case ex: UserException => JsonError(qsiMain.input.id, ex.getMessage, "runtime")
      }
  }
  def contextCom = new {

    var program = programs.programaCom
    var qsiMain = contextOk.qsiMain
    var sim = contextOk.sim
    val arqQ = 5

    qsiMain.program = program
    qsiMain.selectArqQ(arqQ)

    val result =
      try {
        contextQsiMain.multExe.runQsim(sim, qsiMain)
      } catch {
        case ex: UserException => JsonError(qsiMain.input.id, ex.getMessage, "runtime")
      }
  }

  //------------------------------------------------------Caso exitoso------------

  "Un Programa" should "ejecutar exitosamente si esta bien escrito y la arquitectura elegida es la correcta. Tambien se verifica su id." in {
    val (code, out) = contextQsiMain.refereeQsim.evalResult(contextOk.result)
    val idCode = contextQsiMain.jsonInput.id
    val output = out.asInstanceOf[JsonOutOk]

    assert(code.equals(0))
    assert(idCode.equals(output.id))
  }

  // ---------------------------------------------------Caso Erroneo ---------

  "Un Programa" should " ejecutar incorrectamente cuando ocurre un error en su ejecucion: Caso  => Modo Inmediato en Destino" in {
    val(code, out) = contextQsiMain.refereeQsim.evalResult(contextExe.result)
    val idCode = contextQsiMain.jsonInput.id
    val output = out.asInstanceOf[JsonError]

    assert(code.equals(-1))
    assert(idCode.equals(output.id))
    //assert(output.error.equals("Un Inmediato no puede ser un operando destino."))
  }

  // ---------------------------------------------------Caso Compilacion---------

  "Un Programa" should " ejecutar incorrectamente cuando ocurre un error de compilacion : Caso => llamar a una rutina que no existe" in {
    val (code, out) = contextQsiMain.refereeQsim.evalResult(contextCom.result)
    val idCode = contextQsiMain.jsonInput.id
    val output = out.asInstanceOf[JsonError]
    assert(code.equals(-1))
    assert(idCode.equals(output.id))
    //assert(output.error.equals("Una de las etiquetas utilizadas es invalida"))
  }
}