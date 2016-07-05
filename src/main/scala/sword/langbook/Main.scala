package sword.langbook

import sword.db._
import sword.langbook.db._
import sword.langbook.db.registers.WordRepresentation

object Main {

  /**
   * Sample using the storageManager for writing values and the LinkedStorageManager to retrieve
   * them.
   */
  def main(args :Array[String]) :Unit = {
    val manager = LinkedStorageManager(regDefs => new MemoryStorageManager(regDefs))

    for {
      alphabet <- Concept.from(manager, "Latin alphabet").flatMap(Alphabet.from(manager, _))
      language <- Concept.from(manager, "Spanish").flatMap(Language.from(manager, _, "es", alphabet))
      symbolArray <- SymbolArray.from(manager, "carta")
      word <- Word.from(manager, language)
      repr <- manager.storageManager.insert(WordRepresentation(word.key, alphabet.key, symbolArray.arrayId))
    } {
      Concept.from(manager, "letter").foreach(word.concepts += _)
      println(s"Language: $language, Alphabet: $alphabet => ${word.text(alphabet)}")
    }
  }
}

