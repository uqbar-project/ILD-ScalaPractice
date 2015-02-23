package org.uqbar.thin.ild.practice

import scala.util.parsing.combinator._

object NicerParser extends RegexParsers {

	lazy protected val instruction = nop | add | sub | mul | div | swap | lodv | ifnz
	lazy protected val nop = "0" ^^ { _ => NOP }
	lazy protected val add = "1" ^^ { _ => ADD }
	lazy protected val sub = "2" ^^ { _ => SUB }
	lazy protected val mul = "3" ^^ { _ => MUL }
	lazy protected val div = "4" ^^ { _ => DIV }
	lazy protected val swap = "5" ^^ { _ => SWAP }
	lazy protected val lodv = "6" ~> ".".r ^^ { arg => LODV(arg.head) }
	lazy protected val ifnz: Parser[Instruccion] = "9" ~> instruction.* <~ "A" ^^ IFNZ

	def apply(input: String): List[Instruccion] = parseAll(instruction.*, input) match {
		case Success(result, _) => result
		case NoSuccess(msg, _) => throw ParserException(msg)
	}
}