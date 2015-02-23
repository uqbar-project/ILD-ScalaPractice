package org.uqbar.thin.ild.practice

import java.util.Date

/*
 * Resuelve el ejercicio N° 2 de la guía 5 de Objetos, de Paradigmas de Programación de UTN
 * http://www.pdep.com.ar/material/guas-de-ejercicios/guia-objetos-2008-5-2.pdf?attredirects=0
 */

class Mascota(protected var estado: EstadoMascota) {
	def comer = estado = estado match {
		case Hambrienta => Contenta()
		case Contenta(n) => Contenta(n + 1)
		case a: Aburrida if a.minutos > 80 => Contenta()
		case otro => otro
	}

	def jugar = estado = estado match {
		case Contenta(n) => Contenta(n + 2)
		case _: Aburrida => Contenta()
		case otro => otro
	}

	def puedeJugar = estado.puedeJugar

	def estadoActual = estado
}

//*********************************************************************************************
// ESTADOS
//*********************************************************************************************

sealed trait EstadoMascota {
	def puedeJugar = true
}

case object Hambrienta extends EstadoMascota {
	override def puedeJugar = false
}
case class Contenta(nivel: Int = 1) extends EstadoMascota
case class Aburrida(desde: Long = System.currentTimeMillis) extends EstadoMascota {
	def minutos = (System.currentTimeMillis - desde) / 1000 / 60
}