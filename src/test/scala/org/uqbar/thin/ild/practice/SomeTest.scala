package org.uqbar.thin.ild.practice

import org.scalatest.FreeSpec
import org.scalatest.Matchers


class SomeTest extends FreeSpec with Matchers {
	"Math" - {
		"should work" in { 1 + 2 should be (3) }
		"should not break" in { 1 + 2 should not be (5) }
	}
	
	"Salutator should salute" in {
		Salutator() should be ("Hello, World!")
	}
}