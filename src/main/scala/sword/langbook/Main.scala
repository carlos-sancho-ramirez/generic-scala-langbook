package sword.langbook

import sword.db.MemoryStorageManager
import sword.langbook.db.registers
import sword.langbook.db.registers._

object Main {
  def main(args :Array[String]) :Unit = {
    val definitions = List(Alphabet, Concept, Language, LanguageAlphabet, Piece,
      registers.PiecePosition, registers.Symbol, registers.SymbolPosition, registers.Word, registers.WordConcept)
    val manager = new MemoryStorageManager(definitions)

    val conceptOpt = manager.insert(Concept("English"))
    conceptOpt.flatMap(concept => manager.get(concept)) match {
      case Some(Concept(hint)) => println(s"Concept hint is $hint")
      case _ => println("Something went wrong...")
    }
  }
}

