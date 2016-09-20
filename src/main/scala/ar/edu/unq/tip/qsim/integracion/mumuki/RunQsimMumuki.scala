package ar.edu.unq.tip.qsim.integracion.mumuki

import com.google.gson.GsonBuilder

object runMainMumuki extends App {

  val program = args(0)
  val arqQ = args(1).toInt - 1
  val input = args(2)
  val gson = new GsonBuilder().setPrettyPrinting().create()

  val multExe = new MultExecutor()
  val outputs = multExe.exeInputs(program, arqQ, input)
  Console.println(gson.toJson(outputs))
  System.exit(0)
}
