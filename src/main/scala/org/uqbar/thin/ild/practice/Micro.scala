package org.uqbar.thin.ild.practice

trait Instruccion
case object NOP extends Instruccion
case object ADD extends Instruccion
case object SUB extends Instruccion
case object MUL extends Instruccion
case object DIV extends Instruccion
case object SWAP extends Instruccion
case class LODV(arg: Char) extends Instruccion
case class IFNZ(body: List[Instruccion]) extends Instruccion

case class ParserException(description: String) extends RuntimeException(description)

object Parser {
	def apply(input: String): List[Instruccion] = {

		def cortarBody(target: List[Char]): (List[Char], List[Char]) = {
			def cortarBodyR(visto: List[Char], restante: List[Char], abiertos: Int): (List[Char], List[Char]) = restante match {
				case 'A' :: t if abiertos == 0 => (visto, t)
				case 'A' :: t => cortarBodyR(visto :+ 'A', t, abiertos - 1)
				case '9' :: t => cortarBodyR(visto :+ '9', t, abiertos + 1)
				case c :: t => cortarBodyR(visto :+ c, t, abiertos)
			}

			cortarBodyR(Nil, target, 0)
		}

		def parsear(input: List[Char]): List[Instruccion] = input match {
			case Nil => Nil
			case '0' :: t => NOP :: parsear(t)
			case '1' :: t => ADD :: parsear(t)
			case '2' :: t => SUB :: parsear(t)
			case '3' :: t => MUL :: parsear(t)
			case '4' :: t => DIV :: parsear(t)
			case '5' :: t => SWAP :: parsear(t)
			case '6' :: a :: t => LODV(a) :: parsear(t)
			case '9' :: t =>
				val (body, restoDesdeQueTerminaElBody) = cortarBody(t)
				IFNZ(parsear(body)) :: parsear(restoDesdeQueTerminaElBody)
			case otro :: _ => throw new ParserException(s"El código $otro no está asociado a ninguna instrucción")
		}

		parsear(input.toList)
	}
}