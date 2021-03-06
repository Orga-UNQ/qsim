package ar.edu.unq.tpi.qsim.model

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

import scala.collection.JavaConversions._
import scala.collection.mutable._
import ar.edu.unq.tpi.qsim.utils.Util

class BusEntradaSalida {

  var memoria: Memoria = _
  var puertos: CeldasPuertos = _
  var stateMemory: java.util.Map[String, String] = Map[String, String]()
  
  def initialize() {
    memoria = Memoria(65536)
    memoria.initialize
    puertos = new CeldasPuertos()
    puertos.initialize()
  }

  /**
   * Recibe un entero y devuelve el valor de la celda en memoria o de puertos segun corresponda en esa posicion.
    *
    * @param Int
   * @return W16
   */
  def getValor(pc: Int): W16 = {
    if ((pc >= 65520) && (pc <= 65535)) {
      this.puertos.getValor(pc)
    } else { this.memoria.getValor(pc) }
  }

  /**
   * Recibe un W16 y devuelve el valor de la celda en memoria o de puertos segun corresponda en esa posicion.
    * @param W16
   * @return W16
   */
  def getValor(pc: W16): W16 = {
    getValor(pc.value)
  }

  /**
   * Recibe un String en Hexadecimal y devuelve el valor de la celda en memoria o de puertos segun corresponda en esa posicion.
    *
    * @param String
   * @return W16
   */
  def getValor(pc: String): W16 = {
    getValor(Util.hexToInteger(pc))
  }

  /**
   * Pone un valor (W16) en la celda de memoria o reservada para puertos segun corresponda que se le indica por parametro.
    *
    * @param Int, W16
   */
  def setValorC(celda: Int, dato: W16) = {
    if ((celda >= 65520) && (celda <= 65535)) {
      this.puertos.setValorC(celda, dato)
    } else { this.memoria.setValorC(celda, dato) }
  }

  /**
   * Pone un valor (W16) en la celda de memoria o reservada para puertos segun corresponda que se le indica por parametro en valor hexadecimal.
    *
    * @param String, W16
   */
  def setValor(celda: String, valor: W16) = setValorC(Util.hexToInteger(celda), valor)

  /**
   * Cambia el estado de una celda de memoria o reservada para puertos por el pasado por parametro.
    *
    * @param Int, Int
   */
  def setStateCelda(num_celda: Int, state: State.Type) = {
    if ((num_celda >= 65520) && (num_celda <= 65535)) {
      this.puertos.setStateCelda(num_celda, state)
    } else { this.memoria.setStateCelda(num_celda, state) }
  }

  def setStateToMemory(stateMem: java.util.Map[String, String]) {
    for (celda <- stateMem.keys) {
      var dir = new W16(celda)
      var value = stateMem.get(celda)
      memoria.setValor(dir.hex, new W16(value))
      dir.++
    }
  }

  /**
    * Esta funcion recibe como parametros el estado de la memoria para generar un diccionario ue permita mostrar dicho estado de manera simple.
    */
  def getStateOfMemory() = {
    var contador = 0
    var celdaContador = new W16("0000")
    var sizeMemory = memoria.tamanioMemoria()

    do {
      if (!memoria.isEmpty(celdaContador)) {
        stateMemory(celdaContador.hex) = memoria.getValor(celdaContador).hex
      }
      celdaContador.++
      contador = contador + 1
    } while (contador < sizeMemory)
    stateMemory
  }
}