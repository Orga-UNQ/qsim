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

import ar.edu.unq.tpi.qsim.utils.Util
import scala.collection.mutable._
import com.google.gson.Gson

class BusEntradaSalida {

  var memoria: Memoria = _
  var puertos: CeldasPuertos = _
  var stateMemory: Map[String, Map[String, String]] = Map[String, Map[String, String]]()
  def initialize() {
    memoria = Memoria(65536)
    //memoria = Memoria(736)
    memoria.initialize
    puertos = new CeldasPuertos()
    puertos.initialize
  }

  /**
   * Recibe un entero y devuelve el valor de la celda en memoria o de puertos segun corresponda en esa posicion.
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
   * @param String
   * @return W16
   */
  def getValor(pc: String): W16 = {
    getValor(Util.hexToInteger(pc))
  }

  /**
   * Pone un valor (W16) en la celda de memoria o reservada para puertos segun corresponda que se le indica por parametro.
   * @param Int, W16
   */
  def setValorC(celda: Int, dato: W16) = {
    if ((celda >= 65520) && (celda <= 65535)) {
      this.puertos.setValorC(celda, dato)
    } else { this.memoria.setValorC(celda, dato) }
  }

  /**
   * Pone un valor (W16) en la celda de memoria o reservada para puertos segun corresponda que se le indica por parametro en valor hexadecimal.
   * @param String, W16
   */
  def setValor(celda: String, valor: W16) = setValorC(Util.hexToInteger(celda), valor)

  /**
   * Cambia el estado de una celda de memoria o reservada para puertos por el pasado por parametro.
   * @param Int, Int
   */
  def setStateCelda(num_celda: Int, state: State.Type) = {
    if ((num_celda >= 65520) && (num_celda <= 65535)) {
      this.puertos.setStateCelda(num_celda, state)
    } else { this.memoria.setStateCelda(num_celda, state) }
  }

  def setStateToMemory(stateMem: Map[String, Map[String, String]]) {
    for (key <- stateMem.keys) {
      Console.println("Que es key")
      Console.println(key)
      var fila16 = stateMem.get(key).get
      Console.println("Que es fila16")
      Console.println(fila16)
      for (fkey <- fila16.keys) {
        Console.println("Que es fkey")
        Console.println(fkey)
        var value = fila16.get(fkey).get
        Console.println("Que es value")
        Console.println(value)
        memoria.setValor(fkey, new W16(value))
      }
    }
  }

  /**
   * Esta funcion recibe como parametros el estado de la memoria para generar un diccionario ue permita mostrar dicho estado de manera simple.
   */
  def getStateOfMemory(): Map[String, Map[String, String]] = {
    var contador = 0
    var filaActual = 15
    var celdaInit = new W16("0000")
    var celdaContador = new W16("0000")
    var sizeMemory = 17 //memoria.tamanioMemoria()
    var fila16celdas = Map[String, String]()

    //    Console.println("Contador:::::")
    //    Console.println(contador)
    //    Console.println("Fila Actual:::::")
    //    Console.println(filaActual)
    //    Console.println("Celda Inicial::::")
    //    Console.println(celdaInit.hex)
    //    Console.println("Celda Contador::::::")
    //    Console.println(celdaContador.hex)
    //    Console.println("State Memory::::::")
    //    Console.println(stateMemory.toString())
    //    Console.println("Fila 16 celdas::::::")
    //    Console.println(fila16celdas.toString())
    //
    do {

      //      Console.println("Dentro del while Contador:::::")
      //      Console.println(contador)1
      //
      //      Console.println("Dentro del while Fila Actual:::::")
      //      Console.println(filaActual)
      //
      //      Console.println("Dentro del while Celda contador:::::")
      //      Console.println(celdaContador)
      //
      //      Console.println("Dentro del while Celda Init:::::")
      //      Console.println(celdaInit)

      if (contador <= filaActual) {
        //        Console.println("se cumplio condicion contador < que fila actual")
        fila16celdas = fila16celdas + (celdaContador.toString() -> memoria.getValor(celdaContador).toString())
        //        Console.println("add Fila 16 celdas::::::")
        //        Console.println(fila16celdas.toString())
      } else {
        stateMemory = stateMemory + (celdaInit.hex -> fila16celdas)
        //        Console.println("add complete file in State Memory::::::")
        //        Console.println(stateMemory.toString())

        fila16celdas = Map[String, String]()
        //        Console.println("Nueva Fila 16 celdas::::::\n")
        //        Console.println(fila16celdas.toString())

        celdaInit.++(16)
        //        Console.println("Actualizando Celda Inicial::::")
        //        Console.println(celdaInit.hex)

        filaActual = filaActual + filaActual
        //        Console.println("Actualizando Fila Actual:::::")
        //        Console.println(filaActual.toString())

        fila16celdas = fila16celdas + (celdaContador.toString() -> memoria.getValor(celdaContador).toString())
        //        Console.println("Verificando que Celda Contador sea el correcto::::::")
        //        Console.println(celdaContador.hex)
        //        Console.println("Add celda a Fila 16 celdas::::::")
        //        Console.println(fila16celdas.toString())

      }
      celdaContador.++
      contador = contador + 1
    } while (contador < sizeMemory)
    stateMemory
  }
}

//object Testa extends App {
//  var bus = new BusEntradaSalida()
//  bus.initialize()
//  bus.crearFila16Celdas()
//  //println(t)
//  //val a = Util.toHex4(1)
//  //println(a)
//}
