package ar.edu.unq.tip.qsim.integracion.mumuki

import ar.edu.unq.tpi.qsim.integracion.mumuki.{JsonOutPut, JsonError, JsonOutOk}

/**
  * Created by susanrosito on 8/13/16.
  */
class RefereeQsimMumuki {

  def evalResult(result: JsonOutPut): (Int, JsonOutPut) = {
    val exitCode = result match {
      case ok: JsonOutOk => (0, result)
      case error: JsonError => (-1, result)
    }
    exitCode
  }

  def getResultOk(result: JsonOutPut): JsonOutOk =  {
    val (_, out) = this.evalResult(result)
    val exit = out match {
      case ok: JsonOutOk => ok
    }
    exit
  }

  def getResultError(result: JsonOutPut): JsonError =  {
    val (_, out) = this.evalResult(result)
    val exit = out match {
      case error: JsonError => error
    }
    exit
  }

  def getExit(result: JsonOutPut) = {
    val (code, _) = this.evalResult(result)
    code
  }
}
