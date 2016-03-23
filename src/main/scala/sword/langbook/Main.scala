package sword.langbook

import sword.db._
import sword.langbook.db._

object Main {

  /**
   * Sample using the storageManager for writing values and the LinkedStorageManager to retrieve
   * them.
   */
  def main(args :Array[String]) :Unit = {
    val manager = LinkedStorageManager(regDefs => new MemoryStorageManager(regDefs))

    for {
      alphabet <- Concept.from(manager, "Latin alphabet").flatMap(Alphabet.from(manager, _))
      language <- Concept.from(manager, "Spanish").flatMap(Language.from(manager, _, alphabet))
      symbolArray <- SymbolArray.from(manager, "carta")
      cartaPiece <- Piece.from(manager, alphabet, symbolArray)
      cartaPieceArray <- PieceArray.from(manager, List(cartaPiece))
      word <- Word.from(manager, language, cartaPieceArray)
    } {
      Concept.from(manager, "letter").foreach(word.concepts += _)
      println(s"Language: $language, Alphabet: $alphabet => ${word.text(alphabet)}")
    }
  }
}

