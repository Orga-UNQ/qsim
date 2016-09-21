package ar.edu.unq.tip.qsim.integracion.mumuki

import ar.edu.unq.tpi.qsim.integracion.mumuki.{JsonOutPut, JsonError, JsonOutOk}

/**
  * Created by susanrosito on 8/13/16.
  */
class RefereeQsimMumuki {

  def evalResult(result: JsonOutPut): (Int, JsonOutPut) = {
    result match {
      case ok: JsonOutOk => (0, result)
      case error: JsonError => (-1, result)
    }
  }
}
