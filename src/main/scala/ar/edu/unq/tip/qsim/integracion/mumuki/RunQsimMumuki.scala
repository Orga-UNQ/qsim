package ar.edu.unq.tip.qsim.integracion.mumuki

object runMainMumuki extends App {

  var program = args(0)
  var arqQ = args(1).toInt - 1
  var input = args(2)

  var multExe = new MultExecutor()
  multExe.exeInputs(program, arqQ, input)
  System.exit(0)
}
