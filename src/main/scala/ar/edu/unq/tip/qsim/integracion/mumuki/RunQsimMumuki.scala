package ar.edu.unq.tip.qsim.integracion.mumuki

import ar.edu.unq.tpi.qsim.model.Simulador
import ar.edu.unq.tpi.qsim.model.W16
import scala.collection.mutable.Map
import ar.edu.unq.tpi.qsim.exeptions.{RuntimeErrorException,SyntaxErrorException}
import org.uqbar.commons.model.UserException
import com.google.gson.GsonBuilder
import ar.edu.unq.tpi.qsim.integracion.mumuki.JsonError

object runMainMumuki extends App {

  var program = args(0)
  var arqQ = args(1).toInt - 1
  var la = new QsimMainMumuki()
  var sim = Simulador()
  var refereeQsim = new RefereeQsimMumuki()

  val result = 
    try {
      la.setPathFile(program)
      la.selectArqQ(arqQ)
      la.ensamblar()
      sim.inicializarSim()
      sim.cargarProgramaYRegistros(la.program, "0000", Map[String, W16]())
      sim.execute_all_program()
    } catch {
      case ex: SyntaxErrorException => JsonError(ex.getMessage, "syntax")
      case ex: RuntimeErrorException => JsonError(ex.getMessage, "runtime")
      case ex: UserException => JsonError(ex.getMessage, "runtime")
      case ex: Throwable => JsonError(ex.getMessage, "unknown")
    }

  val (code, output) = refereeQsim.evalResult(result)

  var gson = new GsonBuilder().setPrettyPrinting().create()

  Console.println(gson.toJson(output))

  System.exit(code)
} 
