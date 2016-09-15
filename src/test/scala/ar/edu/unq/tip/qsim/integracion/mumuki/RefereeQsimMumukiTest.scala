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

import ar.edu.unq.tpi.qsim.exeptions.{ RuntimeErrorException, SyntaxErrorException}
import ar.edu.unq.tpi.qsim.integracion.mumuki.{ JsonOutOk, JsonError }
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ar.edu.unq.tpi.qsim.model._
import org.uqbar.commons.model.UserException
import scala.collection.mutable._
import ar.edu.unq.tpi.qsim.utils._
import scala.collection.JavaConversions._

class RefereeQsimMumukiTest extends FlatSpec with Matchers {

  def programaHap = new {

    var inst1 = MOV(R3, new Inmediato("0003"))
    var inst2 = MOV(R5, new Inmediato("0004"))
    var inst3 = ADD(R3, R5)

    var programa = new Programa(List(inst1, inst2, inst3))
  }

  def programaExe = new {

    var inst1 = MOV(new Inmediato("0002"), new Inmediato("0003"))
    var inst2 = MOV(R5, new Inmediato("0004"))
    var inst3 = ADD(R3, R5)

    var programa = new Programa(List(inst1, inst2, inst3))
  }

  def programaCom = new {

    var inst1 = CALL(new Etiqueta("sumarDos"))
    var inst2 = MOV(R2, new Inmediato("0004"))
    var inst3 = ADD(R2, R5)

    var programa = new Programa(List(inst1, inst2, inst3))
  }

  def contextoProgramaOk = new {

    var programa = programaHap.programa
    var arqQ = 0 // ArquitecturaQ1
    var la = new QsimMainMumuki()
    var sim = Simulador()
    var refereeQsim = new RefereeQsimMumuki()
    var flags = Map[String, Any]("v" -> 0, "c" -> 0, "z" -> 0, "n" -> 0)
    var posMemory: java.util.Map[String, String] = Map[String, String]("1002" -> "000A")
    var multExe = new MultExecutor()

    val result =
      try {
        la.selectArqQ(arqQ)
        la.ensamblar()
        sim.inicializarSim(flags, posMemory)
        sim.cargarProgramaYRegistros(programa, "0000", Map[String, W16]())
        sim.execute_all_program()
      } catch {
        case ex: SyntaxErrorException  => JsonError(ex.getMessage, "syntax")
        case ex: RuntimeErrorException => JsonError(ex.getMessage, "runtime")
        case ex: UserException         => JsonError(ex.getMessage, "runtime")
        case ex: Throwable             => JsonError(ex.getMessage, "unknown")
      }
  }

  def contextoProgramaExe = new {

    var programa = programaExe.programa
    var arqQ = 0 // ArquitecturaQ1
    var la = new QsimMainMumuki()
    var sim = Simulador()
    var refereeQsim = new RefereeQsimMumuki()
    var flags = Map[String, Any]("v" -> 0, "c" -> 0, "z" -> 0, "n" -> 0)
    var posMemory: java.util.Map[String, String] = Map[String, String]("1001" -> "0002")

    val result =
      try {
        la.selectArqQ(arqQ)
        la.ensamblar()
        sim.inicializarSim(flags, posMemory)
        sim.cargarProgramaYRegistros(programa, "0000", Map[String, W16]())
        sim.execute_all_program()
      } catch {
        case ex: SyntaxErrorException  => JsonError(ex.getMessage, "syntax")
        case ex: RuntimeErrorException => JsonError(ex.getMessage, "runtime")
        case ex: UserException         => JsonError(ex.getMessage, "runtime")
        case ex: Throwable             => JsonError(ex.getMessage, "unknown")
      }
  }

  def contextoCargarInput = new {

    var programa = programaCom.programa
    var arqQ = 5 // ArquitecturaQ1
    var la = new QsimMainMumuki()
    var sim = Simulador()
    var refereeQsim = new RefereeQsimMumuki()
    var flags = Map[String, Any]("v" -> 0, "c" -> 0, "z" -> 0, "n" -> 0)
    var posMemory: java.util.Map[String, String] = Map[String, String]("1000" -> "0002", "1001" -> "0003", "1002" -> "0004", "1003" -> "0005")

    try {
      la.selectArqQ(arqQ)
      la.ensamblar()
      sim.inicializarSim(flags, posMemory)
    } catch {
      case ex: UserException => JsonError(ex.getMessage, "runtime")
      case ex: Throwable     => JsonError(ex.getMessage, "unknown")
    }

  }

  def contextoProgramaCom = new {

    var programa = programaCom.programa
    var arqQ = 5 // ArquitecturaQ1
    var la = new QsimMainMumuki()
    var sim = Simulador()
    var refereeQsim = new RefereeQsimMumuki()
    var flags = Map[String, Any]("v" -> 0, "c" -> 0, "z" -> 0, "n" -> 0)
    var posMemory: java.util.Map[String, String] = Map[String, String]("1000" -> "0005")

    val result =
      try {
        la.selectArqQ(arqQ)
        la.ensamblar()
        sim.inicializarSim(flags, posMemory)
        sim.cargarProgramaYRegistros(programa, "0000", Map[String, W16]())
        sim.execute_all_program()
      } catch {
        case ex: SyntaxErrorException  => JsonError(ex.getMessage, "syntax")
        case ex: RuntimeErrorException => JsonError(ex.getMessage, "runtime")
        case ex: UserException         => JsonError(ex.getMessage, "runtime")
        case ex: Throwable             => JsonError(ex.getMessage, "unknown")
      }
  }

  //------------------------------------------------------Caso exitoso------------

  "Un Programa" should "ejecutar exitosamente si esta bien escrito y la arquitectura elegida es la correcta" in {
    var ctx = contextoProgramaOk
    val (code, output) = ctx.refereeQsim.evalResult(ctx.result)

    assert(code.equals(0))
    assert(output.asInstanceOf[JsonOutOk].special_records.PC.equals(ctx.sim.cpu.pc.hex))
    assert(output.asInstanceOf[JsonOutOk].special_records.SP.equals(ctx.sim.cpu.sp.hex))
    assert(output.asInstanceOf[JsonOutOk].special_records.IR.equals(ctx.sim.cpu.ir))

    assert(output.asInstanceOf[JsonOutOk].flags.C.equals(ctx.sim.cpu.c))
    assert(output.asInstanceOf[JsonOutOk].flags.N.equals(ctx.sim.cpu.n))
    assert(output.asInstanceOf[JsonOutOk].flags.V.equals(ctx.sim.cpu.v))
    assert(output.asInstanceOf[JsonOutOk].flags.Z.equals(ctx.sim.cpu.z))

    assert(output.asInstanceOf[JsonOutOk].records.R0.equals(ctx.sim.cpu.registro("R0").get.getValor().hex))
    assert(output.asInstanceOf[JsonOutOk].records.R1.equals(ctx.sim.cpu.registro("R1").get.getValor().hex))
    assert(output.asInstanceOf[JsonOutOk].records.R2.equals(ctx.sim.cpu.registro("R2").get.getValor().hex))
    assert(output.asInstanceOf[JsonOutOk].records.R3.equals(ctx.sim.cpu.registro("R3").get.getValor().hex))
    assert(output.asInstanceOf[JsonOutOk].records.R4.equals(ctx.sim.cpu.registro("R4").get.getValor().hex))
    assert(output.asInstanceOf[JsonOutOk].records.R5.equals(ctx.sim.cpu.registro("R5").get.getValor().hex))
    assert(output.asInstanceOf[JsonOutOk].records.R6.equals(ctx.sim.cpu.registro("R6").get.getValor().hex))
    assert(output.asInstanceOf[JsonOutOk].records.R7.equals(ctx.sim.cpu.registro("R7").get.getValor().hex))
  }

  // ---------------------------------------------------Caso Erroneo ---------

  "Un Programa" should " ejecutar incorrectamente cuando ocurre un error en su ejecucion: Caso  => Modo Inmediato en Destino" in {
    val ctx = contextoProgramaExe
    val (code, output) = ctx.refereeQsim.evalResult(ctx.result)

    assert(code.equals(-1))

    assert(output.asInstanceOf[JsonError].error.equals("Un Inmediato no puede ser un operando destino."))
  }

  // ---------------------------------------------------Caso Compilacion---------

  "Un Programa" should " ejecutar incorrectamente cuando ocurre un error de compilacion : Caso => llamar a una rutina que no existe" in {
    val ctx = contextoProgramaCom
    val (code, output) = ctx.refereeQsim.evalResult(ctx.result)

    assert(code.equals(-1))

    assert(output.asInstanceOf[JsonError].error.equals("Una de las etiquetas utilizadas es invalida"))
  }

  // Verificacion de la memoria, si los datos se guardan bien en los espacios que se indican en el input

  "El Simulador" should " cargar correctamente el estado inicial de cada celda de memoria que se indica en el archivo input" in {
    val ctx = contextoCargarInput

    for (celda <- ctx.posMemory.keySet()) {
      var valueIn = ctx.posMemory.get(celda)
      var valueM = ctx.sim.busIO.memoria.getValor(celda).hex
      assert(valueIn.equals(valueM))
    }
  }

  // Verificacion de la memoria, si los datos se guardan bien en los espacios que se indican en el input

  "El Simulador" should " devolver el estado total de la memoria, osea de cada celda (su estado final)" in {
    val ctx = contextoProgramaOk
    val (code, output) = ctx.refereeQsim.evalResult(ctx.result)

    val outMemory = output.asInstanceOf[JsonOutOk].memory
    val memory = ctx.sim.busIO.memoria

    var celdaInit = new W16("0000")
    var celdaCont = new W16("0000")
    var tam = memory.tamanioMemoria()
    var contador = 0
    var idx = 0

    do {
      if (outMemory.containsKey(celdaInit.hex)) {
        if (idx <= 15) {
          var fila16 = outMemory.get(celdaInit.hex)
          var valueOut = fila16.get(idx)
          var valueMem = memory.getValor(celdaCont.hex).hex
          assert(valueOut.equals(valueMem))
          celdaCont.++
        } else {
          celdaInit.++(16)
          idx = 0
          var fila16 = outMemory.get(celdaInit.hex)
          var valueOut = fila16.get(idx)
          var valueMem = memory.getValor(celdaCont.hex).hex
          assert(valueOut.equals(valueMem))
          celdaCont.++
        }
      }
      contador = contador + 1
      idx = idx + 1
    } while (contador < tam)
  }
}