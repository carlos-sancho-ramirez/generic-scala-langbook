package sword.langbook

import sword.langbook.db.Concept

object Main {
  def main(args :Array[String]) :Unit = {
    val definitions = List(db.Alphabet, Concept, db.Language, db.LanguageAlphabet, db.Piece,
      db.PiecePosition, db.Symbol, db.SymbolPosition, db.Word, db.WordConcept)
    val manager = new db.MemoryStorageManager(definitions)

    val conceptOpt = manager.insert(Concept("English"))
    conceptOpt.flatMap(concept => manager.get(Concept, concept)) match {
      case Some(Concept(hint)) => println(s"Concept hint is $hint")
      case _ => println("Something went wrong...")
    }
  }
}

