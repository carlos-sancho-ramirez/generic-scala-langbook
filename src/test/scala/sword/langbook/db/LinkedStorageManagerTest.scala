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

  it should "not repeat field definitions across registers" in {
    val regDefs = newManager.registerDefinitions
    val fields = regDefs.flatMap(_.fields)
    fields.toSet.size shouldBe fields.size
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

  it can "insert a word" in {
    val manager = newManager
    val kanji = Concept.from(manager, "kanji").flatMap(Alphabet.from(manager, _)).get
    val japanese = Concept.from(manager, "Japanese").flatMap(Language.from(manager, _, "ja", kanji)).get
    val word = Word.from(manager, japanese).get
    word.language shouldBe japanese
  }

  it should "recognise words linked to the same concept as synonym only if they belong to the same language" in {
    val manager = newManager
    val latin = Concept.from(manager, "Latin").flatMap(Alphabet.from(manager,_)).get
    val english = Concept.from(manager, "English").flatMap(Language.from(manager, _, "en", latin)).get

    val bigWord = Word.from(manager, english).get
    val largeWord = Word.from(manager, english).get

    bigWord.synonyms shouldBe empty
    largeWord.synonyms shouldBe empty

    bigWord.concepts shouldBe empty
    val concept = Concept.from(manager, "Big").get
    bigWord.concepts += concept
    bigWord.concepts.size shouldBe 1
    bigWord.concepts.head shouldBe concept

    bigWord.synonyms shouldBe empty
    largeWord.synonyms shouldBe empty

    largeWord.concepts shouldBe empty
    largeWord.concepts += concept
    largeWord.concepts.size shouldBe 1
    largeWord.concepts.head shouldBe concept

    bigWord.synonyms should not contain bigWord
    largeWord.synonyms should not contain largeWord

    bigWord.synonyms should contain (largeWord)
    largeWord.synonyms should contain (bigWord)
  }

  it should "recognise words linked to the same concept as synonym only if they belong to the same language (reusing set instances)" in {
    val manager = newManager
    val latin = Concept.from(manager, "Latin").flatMap(Alphabet.from(manager,_)).get
    val english = Concept.from(manager, "English").flatMap(Language.from(manager, _, "en", latin)).get

    val bigWord = Word.from(manager, english).get
    val largeWord = Word.from(manager, english).get

    val bigSynonyms = bigWord.synonyms
    val largeSynonyms = largeWord.synonyms
    val bigConcepts = bigWord.concepts
    val largeConcepts = largeWord.concepts

    bigSynonyms shouldBe empty
    largeSynonyms shouldBe empty
    bigConcepts shouldBe empty

    val concept = Concept.from(manager, "Big").get
    bigConcepts += concept
    bigConcepts.size shouldBe 1
    bigConcepts.head shouldBe concept

    bigSynonyms shouldBe empty
    largeSynonyms shouldBe empty

    largeConcepts shouldBe empty
    largeConcepts += concept
    largeConcepts.size shouldBe 1
    largeConcepts.head shouldBe concept

    bigSynonyms should not contain bigWord
    largeSynonyms should not contain largeWord

    bigSynonyms should contain (largeWord)
    largeSynonyms should contain (bigWord)
  }

  it should "not recognise words linked to the same concept as translation if they belong to the same language" in {
    val manager = newManager
    val latin = Concept.from(manager, "Latin").flatMap(Alphabet.from(manager, _)).get
    val english = Concept.from(manager, "English").flatMap(Language.from(manager, _, "en", latin)).get

    val bigWord = Word.from(manager, english).get
    val largeWord = Word.from(manager, english).get

    bigWord.concepts shouldBe empty
    val concept = Concept.from(manager, "Big").get
    bigWord.concepts += concept
    largeWord.concepts += concept

    bigWord.translations shouldBe empty
    largeWord.translations shouldBe empty
  }

  it should "recognise words linked to the same concept as translations only if they belong to different languages" in {
    val manager = newManager
    val latin = Concept.from(manager, "Latin").flatMap(Alphabet.from(manager,_)).get
    val english = Concept.from(manager, "English").flatMap(Language.from(manager, _, "en", latin)).get
    val spanish = Concept.from(manager, "Spanish").flatMap(Language.from(manager, _, "es", latin)).get

    val enWord = Word.from(manager, english).get
    val spWord = Word.from(manager, spanish).get

    enWord.translations shouldBe empty
    spWord.translations shouldBe empty

    enWord.concepts shouldBe empty
    val concept = Concept.from(manager, "White").get
    enWord.concepts += concept
    enWord.concepts.size shouldBe 1
    enWord.concepts.head shouldBe concept

    enWord.translations shouldBe empty
    spWord.translations shouldBe empty

    spWord.concepts shouldBe empty
    spWord.concepts += concept
    spWord.concepts.size shouldBe 1
    spWord.concepts.head shouldBe concept

    enWord.translations should not contain enWord
    spWord.translations should not contain spWord

    enWord.translations should contain (spWord)
    spWord.translations should contain (enWord)
  }

  it should "recognise words linked to the same concept as translation only if they belong to different languages (reusing set instances)" in {
    val manager = newManager
    val latin = Concept.from(manager, "Latin").flatMap(Alphabet.from(manager,_)).get
    val english = Concept.from(manager, "English").flatMap(Language.from(manager, _,"en",  latin)).get
    val spanish = Concept.from(manager, "Spanish").flatMap(Language.from(manager, _,"es",  latin)).get

    val enWord = Word.from(manager, english).get
    val spWord = Word.from(manager, spanish).get

    val enTranslations = enWord.translations
    val spTranslations = spWord.translations
    val enConcepts = enWord.concepts
    val spConcepts = spWord.concepts

    enTranslations shouldBe empty
    spTranslations shouldBe empty
    enConcepts shouldBe empty

    val concept = Concept.from(manager, "White").get
    enConcepts += concept
    enConcepts.size shouldBe 1
    enConcepts.head shouldBe concept

    enTranslations shouldBe empty
    spTranslations shouldBe empty

    spConcepts shouldBe empty
    spConcepts += concept
    spConcepts.size shouldBe 1
    spConcepts.head shouldBe concept

    enTranslations should not contain enWord
    spTranslations should not contain spWord

    enTranslations should contain (spWord)
    spTranslations should contain (enWord)
  }

  it should "not recognise words linked to the same concept as synonyms if they belong to different languages" in {
    val manager = newManager
    val latin = Concept.from(manager, "Latin").flatMap(Alphabet.from(manager,_)).get
    val english = Concept.from(manager, "English").flatMap(Language.from(manager, _, "en", latin)).get
    val spanish = Concept.from(manager, "Spanish").flatMap(Language.from(manager, _, "es", latin)).get

    val enWord = Word.from(manager, english).get
    val spWord = Word.from(manager, spanish).get

    enWord.concepts shouldBe empty
    val concept = Concept.from(manager, "White").get
    enWord.concepts += concept
    spWord.concepts += concept

    enWord.synonyms shouldBe empty
    spWord.synonyms shouldBe empty
  }
}
