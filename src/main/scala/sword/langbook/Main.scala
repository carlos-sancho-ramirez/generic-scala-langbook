package sword.langbook

import sword.langbook.db.{Alphabet, Concept}

object Main {
  def main(args :Array[String]) :Unit = {
    val definitions = List(db.Alphabet, Concept, db.Language, db.LanguageAlphabet, db.Piece,
      db.PiecePosition, db.Symbol, db.SymbolPosition, db.Word, db.WordConcept)
    val manager = new db.StorageManager(definitions)

    val concept = manager.insert(Concept("English"))
    manager.get(concept, Concept) match {
      case Some(Concept(hint)) => println(s"Concept hint is $hint")
      case _ => println("Something went wrong...")
    }
  }
}

