package sword.langbook.db

import org.scalatest.{FlatSpec, Matchers}
import sword.db.MemoryStorageManager
import sword.langbook.db.registers.WordRepresentation

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
    val language = Language.from(manager, concept, "??", alphabet).get

    val languages = manager.languages
    languages.size shouldBe 1
    languages.values.head shouldBe language
  }

  it must "return the preferred alphabet entered when created" in {
    val manager = newManager
    val concept = Concept.from(manager, "Concept").get
    val alphabet = Alphabet.from(manager, Concept.from(manager, "latin").get).get
    val language = Language.from(manager, concept, "??", alphabet).get

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

  private def checkReturnAllAlphabetsWhenSingleWordEntered(set: Language => scala.collection.Set[Alphabet]): Unit = {
    val manager = newManager
    val latin = Concept.from(manager, "Latin").flatMap(Alphabet.from(manager,_)).get
    val english = Concept.from(manager, "English").flatMap(Language.from(manager, _, "en", latin)).get
    val word = Word.from(manager, english).get
    val symbolArray = SymbolArray.from(manager, "party").get
    manager.storageManager.insert(WordRepresentation(word.key, latin.key, symbolArray.arrayId)) shouldBe defined
    set(english).size shouldBe 1
    set(english).head shouldBe latin
  }

  it should "return all alphabets linked to it when a single word is entered" in {
    checkReturnAllAlphabetsWhenSingleWordEntered(_.alphabets)
  }

  it should "return all alphabets linked to it when a single word is entered (reusing set instance)" in {
    reusingSetInstance(checkReturnAllAlphabetsWhenSingleWordEntered)
  }

  private def checkReturnAlphabetsWhen2WordsWithExcludedAlphabetsEntered(set: Language => scala.collection.Set[Alphabet]): Unit = {
    val manager = newManager
    val hiragana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager,_)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager,_)).get
    val japanese = Concept.from(manager, "japanese").flatMap(Language.from(manager, _, "ja", kanji)).get

    val word1 = Word.from(manager, japanese).get
    val word2 = Word.from(manager, japanese).get

    val suruArray = SymbolArray.from(manager, "する").get
    val imaArray = SymbolArray.from(manager, "今").get
    set(japanese).size shouldBe 1

    manager.storageManager.insert(WordRepresentation(word1.key, hiragana.key, suruArray.arrayId)) shouldBe defined
    set(japanese).size shouldBe 2
    set(japanese).contains(hiragana) shouldBe true
    set(japanese).contains(kanji) shouldBe true

    manager.storageManager.insert(WordRepresentation(word2.key, kanji.key, imaArray.arrayId)) shouldBe defined
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

  private def insertImaHiraganaRepresentaiton(manager: LinkedStorageManager, word: Word, hiragana: Alphabet): Unit = {
    val kanaArray = SymbolArray.from(manager, "いま").get
    manager.storageManager.insert(WordRepresentation(word.key, hiragana.key, kanaArray.arrayId))
  }

  private def insertImaRepresentation(manager: LinkedStorageManager, word: Word, hiragana: Alphabet, kanji: Alphabet): Unit = {
    insertImaHiraganaRepresentaiton(manager, word, hiragana)
    val kanjiArray = SymbolArray.from(manager, "今").get
    manager.storageManager.insert(WordRepresentation(word.key, kanji.key, kanjiArray.arrayId))
  }

  private def checkReturnAlphabetsWhenWordWithMoreThanOneAlphabetEntered(set: Language => scala.collection.Set[Alphabet]): Unit = {
    val manager = newManager
    val hiragana = Concept.from(manager, "Hiragana").flatMap(Alphabet.from(manager, _)).get
    val kanji = Concept.from(manager, "Kanji").flatMap(Alphabet.from(manager, _)).get
    val japanese = Concept.from(manager, "japanese").flatMap(Language.from(manager, _, "ja", kanji)).get
    val word = Word.from(manager, japanese).get
    set(japanese).size shouldBe 1

    insertImaRepresentation(manager, word, hiragana, kanji)
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
    val japanese = Concept.from(manager, "japanese").flatMap(Language.from(manager, _, "ja", kanji)).get
    val word1 = Word.from(manager, japanese).get
    val word2 = Word.from(manager, japanese).get
    set(japanese).size shouldBe 1

    insertImaRepresentation(manager, word1, hiragana, kanji)
    set(japanese).size shouldBe 2
    set(japanese).contains(hiragana) shouldBe true
    set(japanese).contains(kanji) shouldBe true

    insertImaHiraganaRepresentaiton(manager, word2, hiragana)
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
    val japanese = Concept.from(manager, "japanese").flatMap(Language.from(manager, _, "ja", kanji)).get
    val word1 = Word.from(manager, japanese).get
    val word2 = Word.from(manager, japanese).get
    set(japanese).size shouldBe 1

    insertImaHiraganaRepresentaiton(manager, word1, hiragana)
    set(japanese).size shouldBe 2
    set(japanese).contains(hiragana) shouldBe true
    set(japanese).contains(kanji) shouldBe true

    insertImaRepresentation(manager, word2, hiragana, kanji)
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
    val language = Language.from(manager, concept, "??", alphabet).get

    language.alphabets.contains(alphabet) shouldBe true
  }
}
