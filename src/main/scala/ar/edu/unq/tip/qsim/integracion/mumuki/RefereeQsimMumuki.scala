package ar.edu.unq.tip.qsim.integracion.mumuki

import ar.edu.unq.tpi.qsim.integracion.mumuki.{JsonOutPut, JsonError, JsonOk}
import com.google.gson.JsonElement

/**
  * Created by susanrosito on 8/13/16.
  */
class RefereeQsimMumuki {

  def evalResult(result: JsonOutPut): (Int, JsonOutPut ) = {
    val exitCode = result match {
      case _: JsonOk => (0, result)
      case _: JsonError => (-1, result)
    }
    exitCode
  }
}
