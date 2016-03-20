package sword.langbook.db

import org.scalatest.{FlatSpec, Matchers}
import sword.db.MemoryStorageManager

class LinkedStorageManagerTest extends FlatSpec with Matchers {

  private def newManager = {
    LinkedStorageManager(defs => new MemoryStorageManager(defs))
  }

  behavior of "LinkedStorageManager"

  it can "be created" in {
    newManager
  }

  it can "insert a concept" in {
    val manager = newManager
    manager.concepts shouldBe empty

    val conceptOption = Concept.from(manager, "MyConcept")
    conceptOption shouldBe defined

    val concepts = manager.concepts
    concepts.size shouldBe 1
    concepts.values.head shouldBe conceptOption.get
  }

  it can "insert a language with an existing concept" in {
    val manager = newManager
    manager.languages shouldBe empty

    val conceptOption = Concept.from(manager, "Concept")
    conceptOption shouldBe defined

    val languageOption = Language.from(manager, conceptOption.get)
    languageOption shouldBe defined

    val concepts = manager.concepts
    concepts.size shouldBe 1
    concepts.values.head shouldBe conceptOption.get

    val languages = manager.languages
    languages.size shouldBe 1
    languages.values.head shouldBe languageOption.get
  }

  it can "insert an alphabet with an existing concept" in {
    val manager = newManager
    manager.alphabets shouldBe empty

    val conceptOption = Concept.from(manager, "Alphabet")
    conceptOption shouldBe defined

    val alphabetOption = Alphabet.from(manager, conceptOption.get)
    alphabetOption shouldBe defined

    val concepts = manager.concepts
    concepts.size shouldBe 1
    concepts.values.head shouldBe conceptOption.get

    val alphabets = manager.alphabets
    alphabets.size shouldBe 1
    alphabets.values.head shouldBe alphabetOption.get
  }

  it can "insert a symbol" in {
    val manager = newManager
    manager.symbols shouldBe empty

    val symbolOption = Symbol.from(manager, 'a')
    symbolOption shouldBe defined

    val symbols = manager.symbols
    symbols.size shouldBe 1
    symbols.values.head shouldBe symbolOption.get
  }

  private def checkAllSymbolsInserted(manager: LinkedStorageManager, text: String, newCharCount: Int) = {
    val initCharCount = manager.symbols.size

    val arrayOption = SymbolArray.from(manager, text)
    arrayOption shouldBe defined

    val symbols = manager.symbols
    symbols.size shouldBe (initCharCount + newCharCount)

    val unicodes = symbols.values.map(_.unicode).toSet
    for (char <- text) {
      unicodes.contains(char.toInt) shouldBe true
    }

    arrayOption.get.size shouldBe text.length
    arrayOption.get.map(_.unicode).zip(text.map(_.toInt)).foreach {
      case (given, expected) =>
        given shouldBe expected
    }
  }

  it can "insert a symbol array for all different chars" in {
    val manager = newManager
    manager.symbols shouldBe empty

    checkAllSymbolsInserted(manager, "Hola", 4)
  }

  it can "insert a symbol array for some repeated chars" in {
    val manager = newManager
    manager.symbols shouldBe empty

    checkAllSymbolsInserted(newManager, "Hello", 4)
  }

  it should "not add new symbols on adding the same symbol array again" in {
    val manager = newManager
    manager.symbols shouldBe empty

    checkAllSymbolsInserted(manager, "Hola", 4)
    checkAllSymbolsInserted(manager, "Hola", 0)
  }

  it should "only insert missing symbols on adding a second symbol array" in {
    val manager = newManager
    manager.symbols shouldBe empty

    checkAllSymbolsInserted(manager, "Hola", 4)
    checkAllSymbolsInserted(manager, "Hello", 1) // Only 'e' was missing
  }

  it can "insert a piece providing a map with a single entry" in {
    val manager = newManager
    val english = Concept.from(manager, "English").flatMap(Alphabet.from(manager,_)).get
    val homeSymbolArray = SymbolArray.from(manager, "Home").get
    val map = Map[Alphabet, SymbolArray](english -> homeSymbolArray)

    val piece = Piece.from(manager, map).get
    piece.size shouldBe 1

    val entry = piece.head
    entry._1 shouldBe english
    entry._2 shouldBe homeSymbolArray
  }

  it can "insert a piece providing a map with 2 entries" in {
    val manager = newManager

    val kana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager,_)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager,_)).get
    kana should not be kanji

    val umbrellaKanaSymbolArray = SymbolArray.from(manager, "かさ").get
    val umbrellaKanjiSymbolArray = SymbolArray.from(manager, "傘").get
    umbrellaKanaSymbolArray should not be umbrellaKanjiSymbolArray

    val map = Map[Alphabet, SymbolArray](
      kana -> umbrellaKanaSymbolArray,
      kanji -> umbrellaKanjiSymbolArray
    )

    val piece = Piece.from(manager, map).get
    piece.size shouldBe 2
    piece.contains(kana) shouldBe true
    piece.contains(kanji) shouldBe true

    piece(kana) shouldBe umbrellaKanaSymbolArray
    piece(kanji) shouldBe umbrellaKanjiSymbolArray
  }
}
