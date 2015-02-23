package org.uqbar.thin.ild.practice

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class MascotaTest extends FreeSpec with Matchers {
	"Cuando una mascota come" - {
		"Si está hambrienta, se pone contenta" in {
			val mascota = new Mascota(Hambrienta)
			mascota.comer
			mascota.estadoActual should be(Contenta())
		}

		"Si está contenta, su nivel se incrementa en una unidad" in {
			val mascota = new Mascota(Contenta(2))
			mascota.comer
			mascota.estadoActual should be(Contenta(3))
		}

		"Si está aburrida, y hace más de 80 minutos que está aburrida, entonces se pone contenta" in {
			val mascota = new Mascota(Aburrida(System.currentTimeMillis - 1000 * 60 * 81))
			mascota.comer
			mascota.estadoActual should be(Contenta())
		}

		"Si está aburrida desde hace 80 minutos o menos, entonces no le pasa nada, no cambia nada" in {
			val estadoOriginal = Aburrida(System.currentTimeMillis - 1000 * 60 * 20)
			val mascota = new Mascota(estadoOriginal)
			mascota.comer
			mascota.estadoActual should be(estadoOriginal)
		}
	}

	"Cuando una mascota juega" - {
		"Si está contenta, su nivel se incrementa en dos unidades" in {
			val mascota = new Mascota(Contenta(3))
			mascota.jugar
			mascota.estadoActual should be(Contenta(5))
		}

		"Si está aburrida, se pone contenta" in {
			val mascota = new Mascota(Aburrida())
			mascota.jugar
			mascota.estadoActual should be(Contenta())
		}

		"No produce ningún efecto jugar con la mascota si esta hambrienta" in {
			val estadoOriginal = Hambrienta
			val mascota = new Mascota(estadoOriginal)
			mascota.jugar
			mascota.estadoActual should be(estadoOriginal)
		}
	}

	"puede jugar" - {
		"si está contenta" in { new Mascota(Contenta()).puedeJugar should be (true) }
		"si está aburrida" in { new Mascota(Aburrida()).puedeJugar should be (true) }
		"a menos que esté hambrienta" in { new Mascota(Hambrienta).puedeJugar should be (false) }
	}
}