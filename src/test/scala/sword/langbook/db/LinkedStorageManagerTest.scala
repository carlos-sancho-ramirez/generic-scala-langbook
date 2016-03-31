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

  it can "insert a piece" in {
    val manager = newManager
    val english = Concept.from(manager, "English").flatMap(Alphabet.from(manager,_)).get
    val homeSymbolArray = SymbolArray.from(manager, "Home").get
    val piece = Piece.from(manager, english, homeSymbolArray).get
    piece.size shouldBe 1

    val entry = piece.head
    entry._1 shouldBe english
    entry._2 shouldBe homeSymbolArray
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

  it can "insert a piece and inserts another" in {
    val manager = newManager

    val kana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager,_)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager,_)).get
    kana should not be kanji

    val umbrellaKanaSymbolArray = SymbolArray.from(manager, "かさ").get
    val umbrellaKanjiSymbolArray = SymbolArray.from(manager, "傘").get
    umbrellaKanaSymbolArray should not be umbrellaKanjiSymbolArray

    val piece = Piece.from(manager, kana, umbrellaKanaSymbolArray).get
    piece += ((kanji, umbrellaKanjiSymbolArray))
    piece.size shouldBe 2
    piece.contains(kana) shouldBe true
    piece.contains(kanji) shouldBe true

    piece(kana) shouldBe umbrellaKanaSymbolArray
    piece(kanji) shouldBe umbrellaKanjiSymbolArray
  }

  it can "insert a piece array" in {
    val manager = newManager
    val spanish = Concept.from(manager, "Spanish").flatMap(Alphabet.from(manager,_)).get

    val symbolArray1 = SymbolArray.from(manager, "ca").get
    val symbolArray2 = SymbolArray.from(manager, "sa").get

    val map1 = Map[Alphabet, SymbolArray](spanish -> symbolArray1)
    val map2 = Map[Alphabet, SymbolArray](spanish -> symbolArray2)

    val piece1 = Piece.from(manager, map1).get
    val piece2 = Piece.from(manager, map2).get

    val array = PieceArray.from(manager, List(piece1, piece2)).get
    array.size shouldBe 2
    array(0) shouldBe piece1
    array(1) shouldBe piece2
  }

  it can "insert a word" in {
    val manager = newManager
    val hiragana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager,_)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager,_)).get
    val japanese = Concept.from(manager, "Japanese").flatMap(Language.from(manager, _, "ja", kanji)).get

    val hiraganaArray1 = SymbolArray.from(manager, "て").get
    val hiraganaArray2 = SymbolArray.from(manager, "がみ").get
    val kanjiArray1 = SymbolArray.from(manager, "手").get
    val kanjiArray2 = SymbolArray.from(manager, "紙").get

    val map1 = Map[Alphabet, SymbolArray](
      hiragana -> hiraganaArray1,
      kanji -> kanjiArray1
    )
    val map2 = Map[Alphabet, SymbolArray](
      hiragana -> hiraganaArray2,
      kanji -> kanjiArray2
    )

    val piece1 = Piece.from(manager, map1).get
    val piece2 = Piece.from(manager, map2).get

    val pieceArray = PieceArray.from(manager, List(piece1, piece2)).get
    val word = Word.from(manager, japanese, pieceArray).get

    word.language shouldBe japanese
    word.pieces shouldBe pieceArray
  }

  it should "recognise words linked to the same concept as synonym only if they belong to the same language" in {
    val manager = newManager
    val latin = Concept.from(manager, "Latin").flatMap(Alphabet.from(manager,_)).get
    val english = Concept.from(manager, "English").flatMap(Language.from(manager, _, "en", latin)).get

    val bigPiece = SymbolArray.from(manager, "big").flatMap(Piece.from(manager, latin, _)).get
    val largePiece = SymbolArray.from(manager, "large").flatMap(Piece.from(manager, latin, _)).get

    val bigWord = PieceArray.from(manager, List(bigPiece)).flatMap(Word.from(manager, english, _)).get
    val largeWord = PieceArray.from(manager, List(largePiece)).flatMap(Word.from(manager, english, _)).get

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

    val bigPiece = SymbolArray.from(manager, "big").flatMap(Piece.from(manager, latin, _)).get
    val largePiece = SymbolArray.from(manager, "large").flatMap(Piece.from(manager, latin, _)).get

    val bigWord = PieceArray.from(manager, List(bigPiece)).flatMap(Word.from(manager, english, _)).get
    val largeWord = PieceArray.from(manager, List(largePiece)).flatMap(Word.from(manager, english, _)).get

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

    val bigPiece = SymbolArray.from(manager, "big").flatMap(Piece.from(manager, latin, _)).get
    val largePiece = SymbolArray.from(manager, "large").flatMap(Piece.from(manager, latin, _)).get

    val bigWord = PieceArray.from(manager, List(bigPiece)).flatMap(Word.from(manager, english, _)).get
    val largeWord = PieceArray.from(manager, List(largePiece)).flatMap(Word.from(manager, english, _)).get

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

    val enPiece = SymbolArray.from(manager, "white").flatMap(Piece.from(manager, latin, _)).get
    val spPiece = SymbolArray.from(manager, "blanco").flatMap(Piece.from(manager, latin, _)).get

    val enWord = PieceArray.from(manager, List(enPiece)).flatMap(Word.from(manager, english, _)).get
    val spWord = PieceArray.from(manager, List(spPiece)).flatMap(Word.from(manager, spanish, _)).get

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

    val enPiece = SymbolArray.from(manager, "white").flatMap(Piece.from(manager, latin, _)).get
    val spPiece = SymbolArray.from(manager, "blanco").flatMap(Piece.from(manager, latin, _)).get

    val enWord = PieceArray.from(manager, List(enPiece)).flatMap(Word.from(manager, english, _)).get
    val spWord = PieceArray.from(manager, List(spPiece)).flatMap(Word.from(manager, spanish, _)).get

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

    val enPiece = SymbolArray.from(manager, "white").flatMap(Piece.from(manager, latin, _)).get
    val spPiece = SymbolArray.from(manager, "blanco").flatMap(Piece.from(manager, latin, _)).get

    val enWord = PieceArray.from(manager, List(enPiece)).flatMap(Word.from(manager, english, _)).get
    val spWord = PieceArray.from(manager, List(spPiece)).flatMap(Word.from(manager, spanish, _)).get

    enWord.concepts shouldBe empty
    val concept = Concept.from(manager, "White").get
    enWord.concepts += concept
    spWord.concepts += concept

    enWord.synonyms shouldBe empty
    spWord.synonyms shouldBe empty
  }
}
