package org.uqbar.thin.ild.practice

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class NicerMicroTest extends FreeSpec with Matchers {
	"Mi NicerParser" - {
		"parsea" - {
			"NOP" in {
				NicerParser("0") should be (NOP :: Nil)
			}

			"dos NOP" in {
				NicerParser("00") should be (NOP :: NOP :: Nil)
			}

			"ADD" in {
				NicerParser("1") should be (ADD :: Nil)
			}
			"SUB" in {
				NicerParser("2") should be (SUB :: Nil)
			}
			"MUL" in {
				NicerParser("3") should be (MUL :: Nil)
			}
			"DIV" in {
				NicerParser("4") should be (DIV :: Nil)
			}

			"SWAP" in {
				NicerParser("5") should be (SWAP :: Nil)
			}

			"LODV" in {
				NicerParser("62") should be (LODV('2') :: Nil)
			}

			"LODV seguido de NOP" in {
				NicerParser("620") should be (LODV('2') :: NOP :: Nil)
			}

			"IFNZ" in {
				// LODV('1'), IFNZ, LODV('2'),SWAP,LODV('3'), ADD, END
				NicerParser("619625631A") should be (LODV('1') :: IFNZ(LODV('2') :: SWAP :: LODV('3') :: ADD :: Nil) :: Nil)
			}
		}

		"no parsea fruta" in {
			a[ParserException] should be thrownBy NicerParser("@")
		}
	}
}