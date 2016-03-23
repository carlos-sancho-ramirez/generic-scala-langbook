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

    val concept = Concept.from(manager, "Concept").get
    val alphabet = Alphabet.from(manager, Concept.from(manager, "latin").get).get
    val language = Language.from(manager, concept, alphabet).get

    val languages = manager.languages
    languages.size shouldBe 1
    languages.values.head shouldBe language
  }

  it must "return the preferred alphabet entered when created" in {
    val manager = newManager
    val concept = Concept.from(manager, "Concept").get
    val alphabet = Alphabet.from(manager, Concept.from(manager, "latin").get).get
    val language = Language.from(manager, concept, alphabet).get

    language.preferredAlphabet shouldBe alphabet
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
    val latin = Concept.from(manager, "Latin").flatMap(Alphabet.from(manager,_)).get
    val english = Concept.from(manager, "English").flatMap(Language.from(manager, _, latin)).get
    val piece = SymbolArray.from(manager, "party").flatMap(Piece.from(manager, latin, _)).get
    set(english).size shouldBe 1

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
    val hiragana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager,_)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager,_)).get
    val japanese = Concept.from(manager, "japanese").flatMap(Language.from(manager, _, kanji)).get
    val suruPiece = SymbolArray.from(manager, "する").flatMap(Piece.from(manager, hiragana, _)).get
    val imaPiece = SymbolArray.from(manager, "今").flatMap(Piece.from(manager, kanji, _)).get
    set(japanese).size shouldBe 1

    PieceArray.from(manager, List(suruPiece)).flatMap(Word.from(manager, japanese, _)).get
    set(japanese).size shouldBe 2
    set(japanese).contains(hiragana) shouldBe true
    set(japanese).contains(kanji) shouldBe true

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
    val hiragana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager, _)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager, _)).get
    val japanese = Concept.from(manager, "japanese").flatMap(Language.from(manager, _, kanji)).get
    val piece = imaPiece(manager, hiragana, kanji)
    set(japanese).size shouldBe 1

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
    val hiragana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager,_)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager, _)).get
    val japanese = Concept.from(manager, "japanese").flatMap(Language.from(manager, _, kanji)).get
    val piece = imaPiece(manager, hiragana, kanji)
    val piece2 = SymbolArray.from(manager, "する").flatMap(Piece.from(manager, hiragana, _)).get
    set(japanese).size shouldBe 1

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
    val hiragana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager,_)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager, _)).get
    val japanese = Concept.from(manager, "japanese").flatMap(Language.from(manager, _, kanji)).get
    val piece = imaPiece(manager, hiragana, kanji)
    val piece2 = SymbolArray.from(manager, "する").flatMap(Piece.from(manager, hiragana, _)).get
    set(japanese).size shouldBe 1

    PieceArray.from(manager, List(piece2)).flatMap(Word.from(manager, japanese, _)).get
    set(japanese).size shouldBe 2
    set(japanese).contains(hiragana) shouldBe true
    set(japanese).contains(kanji) shouldBe true

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

  it must "include the preferred alphabet in the alphabets" in {
    val manager = newManager
    val concept = Concept.from(manager, "Concept").get
    val alphabet = Alphabet.from(manager, Concept.from(manager, "latin").get).get
    val language = Language.from(manager, concept, alphabet).get

    language.alphabets.contains(alphabet) shouldBe true
  }
}
