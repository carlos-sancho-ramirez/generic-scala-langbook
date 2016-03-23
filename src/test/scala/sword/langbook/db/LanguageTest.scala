package sword.langbook.db

import org.scalatest.{Matchers, FlatSpec}
import sword.db.MemoryStorageManager

class LanguageTest extends FlatSpec with Matchers {

  private def newManager = {
    LinkedStorageManager(defs => new MemoryStorageManager(defs))
  }

  behavior of "Language"

  it can "be created" in {
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

  private def reusingSetInstance(f: (Language => scala.collection.Set[Alphabet]) => Unit) = {
    var instance: scala.collection.Set[Alphabet] = null
    f { language =>
      if (instance == null) {
        instance = language.alphabets
      }

      instance
    }
  }

  private def checkReturnAllAlphabetsWhenSignleWordEntered(set: Language => scala.collection.Set[Alphabet]): Unit = {
    val manager = newManager
    val english = Concept.from(manager, "English").flatMap(Language.from(manager,_)).get
    val latin = Concept.from(manager, "Latin").flatMap(Alphabet.from(manager,_)).get
    val piece = SymbolArray.from(manager, "party").flatMap(Piece.from(manager, latin, _)).get
    set(english) shouldBe empty

    PieceArray.from(manager, List(piece)).flatMap(Word.from(manager, english, _)).get
    set(english).size shouldBe 1
    set(english).head shouldBe latin
  }

  it should "return all alphabets linked to it when a single word is entered" in {
    checkReturnAllAlphabetsWhenSignleWordEntered(_.alphabets)
  }

  it should "return all alphabets linked to it when a single word is entered (reusing set instance)" in {
    reusingSetInstance(checkReturnAllAlphabetsWhenSignleWordEntered)
  }

  private def checkReturnAlphabetsWhen2WordsWithExcludedAlphabetsEntered(set: Language => scala.collection.Set[Alphabet]): Unit = {
    val manager = newManager
    val japanese = Concept.from(manager, "japanese").flatMap(Language.from(manager,_)).get
    val hiragana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager,_)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager,_)).get
    val suruPiece = SymbolArray.from(manager, "する").flatMap(Piece.from(manager, hiragana, _)).get
    val imaPiece = SymbolArray.from(manager, "今").flatMap(Piece.from(manager, kanji, _)).get
    set(japanese) shouldBe empty

    PieceArray.from(manager, List(suruPiece)).flatMap(Word.from(manager, japanese, _)).get
    set(japanese).size shouldBe 1
    set(japanese).head shouldBe hiragana

    PieceArray.from(manager, List(imaPiece)).flatMap(Word.from(manager, japanese, _)).get
    set(japanese).size shouldBe 2
    set(japanese).contains(hiragana) shouldBe true
    set(japanese).contains(kanji) shouldBe true
  }

  it should "return all alphabets linked to it when 2 words with excluded alphabets are entered" in {
    checkReturnAlphabetsWhen2WordsWithExcludedAlphabetsEntered(_.alphabets)
  }

  it should "return all alphabets linked to it when 2 words with excluded alphabets are entered (reusing set instance)" in {
    reusingSetInstance(checkReturnAlphabetsWhen2WordsWithExcludedAlphabetsEntered)
  }

  private def imaPiece(manager: LinkedStorageManager, hiragana: Alphabet, kanji: Alphabet): Piece = {
    val kanaArray = SymbolArray.from(manager, "いま").get
    val kanjiArray = SymbolArray.from(manager, "今").get
    val map = Map(hiragana -> kanaArray, kanji -> kanjiArray)
    Piece.from(manager, map).get
  }

  private def checkReturnAlphabetsWhenWordWithMoreThanOneAlphabetEntered(set: Language => scala.collection.Set[Alphabet]): Unit = {
    val manager = newManager
    val japanese = Concept.from(manager, "japanese").flatMap(Language.from(manager,_)).get
    val hiragana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager,_)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager,_)).get
    val piece = imaPiece(manager, hiragana, kanji)
    set(japanese) shouldBe empty

    PieceArray.from(manager, List(piece)).flatMap(Word.from(manager, japanese, _)).get
    set(japanese).size shouldBe 2
    set(japanese).contains(hiragana) shouldBe true
    set(japanese).contains(kanji) shouldBe true
  }

  it should "return all alphabets linked to it when a word with more than one alphabet is entered" in {
    checkReturnAlphabetsWhenWordWithMoreThanOneAlphabetEntered(_.alphabets)
  }

  it should "return all alphabets linked to it when a word with more than one alphabet is entered (reusing set instance)" in {
    reusingSetInstance(checkReturnAlphabetsWhenWordWithMoreThanOneAlphabetEntered)
  }

  private def checkReturnAlphabetsWhenFirstWordContainsMoreAlphabetsThanSecond(set: Language => scala.collection.Set[Alphabet]): Unit = {
    val manager = newManager
    val japanese = Concept.from(manager, "japanese").flatMap(Language.from(manager,_)).get
    val hiragana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager,_)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager,_)).get
    val piece = imaPiece(manager, hiragana, kanji)
    val piece2 = SymbolArray.from(manager, "する").flatMap(Piece.from(manager, hiragana, _)).get
    set(japanese) shouldBe empty

    PieceArray.from(manager, List(piece)).flatMap(Word.from(manager, japanese, _)).get
    set(japanese).size shouldBe 2
    set(japanese).contains(hiragana) shouldBe true
    set(japanese).contains(kanji) shouldBe true

    PieceArray.from(manager, List(piece2)).flatMap(Word.from(manager, japanese, _)).get
    set(japanese).size shouldBe 2
    set(japanese).contains(hiragana) shouldBe true
    set(japanese).contains(kanji) shouldBe true
  }

  it should "return all alphabets linked to it when the first word contains more alphabets than a second one" in {
    checkReturnAlphabetsWhenFirstWordContainsMoreAlphabetsThanSecond(_.alphabets)
  }

  it should "return all alphabets linked to it when the first word contains more alphabets than a second one (reusing set instance)" in {
    reusingSetInstance(checkReturnAlphabetsWhenFirstWordContainsMoreAlphabetsThanSecond)
  }

  private def checkReturnAlphabetsWhenFirstWordContainsLessAlphabetsThanSecond(set: Language => scala.collection.Set[Alphabet]) = {
    val manager = newManager
    val japanese = Concept.from(manager, "japanese").flatMap(Language.from(manager,_)).get
    val hiragana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager,_)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager,_)).get
    val piece = imaPiece(manager, hiragana, kanji)
    val piece2 = SymbolArray.from(manager, "する").flatMap(Piece.from(manager, hiragana, _)).get
    set(japanese) shouldBe empty

    PieceArray.from(manager, List(piece2)).flatMap(Word.from(manager, japanese, _)).get
    set(japanese).size shouldBe 1
    set(japanese).head shouldBe hiragana

    PieceArray.from(manager, List(piece)).flatMap(Word.from(manager, japanese, _)).get
    set(japanese).size shouldBe 2
    set(japanese).contains(hiragana) shouldBe true
    set(japanese).contains(kanji) shouldBe true
  }

  it should "return all alphabets linked to it when the first word contains less alphabets than a second one" in {
    checkReturnAlphabetsWhenFirstWordContainsLessAlphabetsThanSecond(_.alphabets)
  }

  it should "return all alphabets linked to it when the first word contains less alphabets than a second one (reusing set instance)" in {
    reusingSetInstance(checkReturnAlphabetsWhenFirstWordContainsLessAlphabetsThanSecond)
  }
}
