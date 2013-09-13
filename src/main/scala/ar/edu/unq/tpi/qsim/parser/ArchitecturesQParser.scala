package ar.edu.unq.tpi.qsim.parser

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical
import ar.edu.unq.tpi.qsim.model._
import scala.collection.mutable.ArrayBuffer
import ar.edu.unq.tpi.qsim.model.Programa

trait ArchitecturesQParser extends StdTokenParsers  with ImplicitConversions {
  type Tokens = StdTokens
  val lexical = new StdLexical
  lexical.reserved ++= List("MOV", "SUB", "ADD", "DIV", "MUL", "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7")
  lexical.delimiters ++= List(",", ";", "[",  "]","0x")
  
  def registers = "R0" ^^^ R0 |    
  				  "R1" ^^^ R1 | 
  				  "R2" ^^^ R2 |
  				  "R3" ^^^ R3 | 
  				  "R4" ^^^ R4 |
  				  "R5" ^^^ R5 | 
  				  "R6" ^^^ R6 | 
  				  "R7" ^^^ R7
  
  def register = registers 
  
  def inmediate = "0x" ~> numericLit ^^ {case direction => Inmediato(new W16(direction))}
  
  def direct = "[" ~> inmediate <~ "]" ^^ {case direction => Directo(direction)}
  
  //def directionIndirect = "[" ~>directionDirect  <~ "]" ^^ {case direction => DirectionIndirect(direction)}
  
  def directionable = register | inmediate | direct 
  
  def asignable = register | direct

  //operaciones
  def instruccions2 = "MOV" | "SUB" | "DIV" | "ADD" | "MUL" 
  
  def instruccions1 = "CALL"
    
  def instruccions0 = "RET"
    

  def instruction2 = instruccions2 ~ asignable ~ ("," ~> directionable ) ^^
    { case ins ~ dir1 ~ dir2 => Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento],classOf[ModoDireccionamiento]).newInstance(dir1, dir2).asInstanceOf[Instruccion_DosOperandos] }

  def instruction1 = instruccions1 ~ asignable  ^^
    { case ins ~ dir1 => Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor(classOf[ModoDireccionamiento]).newInstance(dir1).asInstanceOf[Instruccion_UnOperando] }
  
  def instruction0 = instruccions0 ^^
    { case ins => Class.forName(s"ar.edu.unq.tpi.qsim.model.$ins").getConstructor().newInstance().asInstanceOf[Instruccion_SinOperandos] }
  
  def instructions = instruction0 | instruction1 | instruction2
  
  def program = rep(instructions) ^^ {case instructions => Programa(ArrayBuffer()++instructions)}

  def parse(input: String) = phrase(program)(new lexical.Scanner(input))
}

object QuarqExample extends App with ArchitecturesQParser {

  val theCode = """
		MOV R1, R2;				
	    SUB R2, 0x0023;
	    ADD R0, 0x0255;
	    DIV R3, R0;	 
	    DIV R3, R0;	 
    """

  parse(theCode) match {
    case Success(result, _) => println(result)
    case Failure(msg, i) => println("[Failure] " + s" $msg in $i")
    case Error(msg, i) => println("[Error] " + s" $msg in $i")
  }
}