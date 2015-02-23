package org.uqbar.thin.ild.practice

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class MicroTest extends FreeSpec with Matchers {
	"Mi parser" - {
		"parsea" - {
			"NOP" in {
				Parser("0") should be (NOP :: Nil)
			}

			"dos NOP" in {
				Parser("00") should be (NOP :: NOP :: Nil)
			}

			"ADD" in {
				Parser("1") should be (ADD :: Nil)
			}
			"SUB" in {
				Parser("2") should be (SUB :: Nil)
			}
			"MUL" in {
				Parser("3") should be (MUL :: Nil)
			}
			"DIV" in {
				Parser("4") should be (DIV :: Nil)
			}

			"SWAP" in {
				Parser("5") should be (SWAP :: Nil)
			}

			"LODV" in {
				Parser("62") should be (LODV('2') :: Nil)
			}

			"LODV seguido de NOP" in {
				Parser("620") should be (LODV('2') :: NOP :: Nil)
			}

			"IFNZ" in {
				// LODV('1'), IFNZ, LODV('2'),SWAP,LODV('3'), ADD, END
				Parser("619625631A") should be (LODV('1') :: IFNZ(LODV('2') :: SWAP :: LODV('3') :: ADD :: Nil) :: Nil)
			}
		}

		"no parsea fruta" in {
			a[ParserException] should be thrownBy Parser("@")
		}
	}
}