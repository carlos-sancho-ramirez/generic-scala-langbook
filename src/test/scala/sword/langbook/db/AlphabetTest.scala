package sword.langbook.db

import org.scalatest.{FlatSpec, Matchers}
import sword.db.MemoryStorageManager
import sword.langbook.db.registers.WordRepresentation

class AlphabetTest extends FlatSpec with Matchers {

  private def newManager = {
    LinkedStorageManager(defs => new MemoryStorageManager(defs))
  }

  private def reusingSetInstance(f: (Alphabet => scala.collection.Set[Language]) => Unit) = {
    var instance: scala.collection.Set[Language] = null
    f { alphabet =>
      if (instance == null) {
        instance = alphabet.languages
      }

      instance
    }
  }

  sealed trait InstanceOption
  object underTest extends InstanceOption
  object nonTested extends InstanceOption

  private def checkSet(expected: scala.collection.Set[Language], testedAlphabet: Alphabet, set: Alphabet => scala.collection.Set[Language]): Unit = {
    set(testedAlphabet).size shouldBe expected.size
    set(testedAlphabet).diff(expected) shouldBe empty

    for (expectedLanguage <- expected) {
      set(testedAlphabet).contains(expectedLanguage) shouldBe true
    }
  }

  private def checkReturnExpectedLanguages(preferredAlphabetsInstanceOptions: List[InstanceOption])(set: Alphabet => scala.collection.Set[Language]): Unit = {
    val manager = newManager
    val testedAlphabet = Alphabet.from(manager, Concept.from(manager, "Alphabet under test").get).get
    val otherAlphabet = Alphabet.from(manager, Concept.from(manager, "Other alphabet").get).get
    val preferredAlphabets = preferredAlphabetsInstanceOptions.map {
      case `underTest` => testedAlphabet
      case `nonTested` => otherAlphabet
    }

    set(testedAlphabet) shouldBe empty
    val targetLanguages = scala.collection.mutable.Set[Language]()

    for (thisAlphabet <- preferredAlphabets) {
      val lang = Language.from(manager, Concept.from(manager, "MyLanguage").get, "xx", thisAlphabet).get
      if (thisAlphabet == testedAlphabet) targetLanguages += lang
      checkSet(targetLanguages, testedAlphabet, set)
    }
  }

  private def checkReturnLanguageIncludingWordUsingThisAlphabet(pieceAlphabetsInstanceOption: InstanceOption*)(set: Alphabet => scala.collection.Set[Language]): Unit = {
    val manager = newManager
    val testedAlphabet = Alphabet.from(manager, Concept.from(manager, "Alphabet under test").get).get
    val preferredAlphabet = Alphabet.from(manager, Concept.from(manager, "Preferred Alphabet").get).get
    val languages = for(_ <- pieceAlphabetsInstanceOption) yield {
      Language.from(manager, Concept.from(manager, "Language").get, "xx", preferredAlphabet).get
    }
    set(testedAlphabet) shouldBe empty

    val expectedLanguages = scala.collection.mutable.Set[Language]()
    for ((language, instanceOption) <- languages zip pieceAlphabetsInstanceOption) {
      val pieceAlphabet = instanceOption match {
        case `underTest` => testedAlphabet
        case `nonTested` => preferredAlphabet
      }

      val symbolArray = SymbolArray.from(manager, "Hello").get
      checkSet(expectedLanguages, testedAlphabet, set)

      val word = Word.from(manager, language).get
      manager.storageManager.insert(WordRepresentation(word.key, pieceAlphabet.key,
          symbolArray.arrayId)) shouldBe defined
      instanceOption match {
        case `underTest` =>
          expectedLanguages += language
        case `nonTested` =>
      }
      checkSet(expectedLanguages, testedAlphabet, set)
    }
  }

  behavior of "Alphabet"

  it can "be created" in {
    val manager = newManager
    manager.alphabets shouldBe empty

    val alphabet = Alphabet.from(manager, Concept.from(manager, "Alphabet").get).get
    val alphabets = manager.alphabets
    alphabets.size shouldBe 1
    alphabets.values.head shouldBe alphabet
  }

  it must "return the concept entered when created" in {
    val manager = newManager
    val concept = Concept.from(manager, "MyAlphabet").get
    val alphabet = Alphabet.from(manager, concept).get
    alphabet.concept shouldBe concept
  }

  behavior of "Alphabet.language"

  it must "return the entered language that uses this alphabet as preferred" in {
    checkReturnExpectedLanguages(List(underTest))(_.languages)
  }

  it must "return the entered language that uses this alphabet as preferred (reusing set instances)" in {
    reusingSetInstance(checkReturnExpectedLanguages(List(underTest)))
  }

  it must "not return the only entered language if it does not use this alphabet as preferred" in {
    checkReturnExpectedLanguages(List(nonTested))(_.languages)
  }

  it must "not return the only entered language if it does not use this alphabet as preferred (reusing set instances)" in {
    reusingSetInstance(checkReturnExpectedLanguages(List(nonTested)))
  }

  it must "return only the first entered language when 2 languages are entered and only the first uses this alphabet as preferred" in {
    checkReturnExpectedLanguages(List(underTest, nonTested))(_.languages)
  }

  it must "return only the first entered language when 2 languages are entered and only the first uses this alphabet as preferred (reusing set instances)" in {
    reusingSetInstance(checkReturnExpectedLanguages(List(underTest, nonTested)))
  }

  it must "return only the second entered language when 2 languages are entered and only the second uses this alphabet as preferred" in {
    checkReturnExpectedLanguages(List(nonTested, underTest))(_.languages)
  }

  it must "return only the second entered language when 2 languages are entered and only the second uses this alphabet as preferred (reusing set instances)" in {
    reusingSetInstance(checkReturnExpectedLanguages(List(nonTested, underTest)))
  }

  it must "return both entered languages when 2 languages are entered and both uses this alphabet as preferred" in {
    checkReturnExpectedLanguages(List(underTest, underTest))(_.languages)
  }

  it must "return both entered languages when 2 languages are entered and both uses this alphabet as preferred (reusing set instances)" in {
    reusingSetInstance(checkReturnExpectedLanguages(List(underTest, underTest)))
  }

  it must "return none of the entered languages when 2 languages are entered and neither uses this alphabet as preferred" in {
    checkReturnExpectedLanguages(List(nonTested, nonTested))(_.languages)
  }

  it must "return none of the entered languages when 2 languages are entered and neither uses this alphabet as preferred (reusing set instances)" in {
    reusingSetInstance(checkReturnExpectedLanguages(List(nonTested, nonTested)))
  }

  it must "return any language including a word with a piece using this alphabet" in {
    checkReturnLanguageIncludingWordUsingThisAlphabet(underTest)(_.languages)
  }

  it must "return any language including a word with a piece using this alphabet (reusing set instances)" in {
    reusingSetInstance(checkReturnLanguageIncludingWordUsingThisAlphabet(underTest))
  }

  it must "return no language if none includes a word with a piece using this alphabet" in {
    checkReturnLanguageIncludingWordUsingThisAlphabet(nonTested)(_.languages)
  }

  it must "return no language if none includes a word with a piece using this alphabet (reusing set instances)" in {
    reusingSetInstance(checkReturnLanguageIncludingWordUsingThisAlphabet(nonTested))
  }

  it must "return both languages if 2 languages are entered including both a word with a piece using this alphabet" in {
    checkReturnLanguageIncludingWordUsingThisAlphabet(underTest, underTest)(_.languages)
  }

  it must "return both languages if 2 languages are entered including both a word with a piece using this alphabet (reusing set instances)" in {
    reusingSetInstance(checkReturnLanguageIncludingWordUsingThisAlphabet(underTest, underTest))
  }

  it must "return none of the languages if 2 languages are entered including neither a word with a piece using this alphabet" in {
    checkReturnLanguageIncludingWordUsingThisAlphabet(nonTested, nonTested)(_.languages)
  }

  it must "return none of the languages if 2 languages are entered including neither a word with a piece using this alphabet (reusing set instances)" in {
    reusingSetInstance(checkReturnLanguageIncludingWordUsingThisAlphabet(nonTested, nonTested))
  }

  it must "return the first language if 2 languages are entered but only the first includes a word with a piece using this alphabet" in {
    checkReturnLanguageIncludingWordUsingThisAlphabet(underTest, nonTested)(_.languages)
  }

  it must "return the first language if 2 languages are entered but only the first includes a word with a piece using this alphabet (reusing set instances)" in {
    reusingSetInstance(checkReturnLanguageIncludingWordUsingThisAlphabet(underTest, nonTested))
  }

  it must "return the second language if 2 languages are entered but only the second includes a word with a piece using this alphabet" in {
    checkReturnLanguageIncludingWordUsingThisAlphabet(nonTested, underTest)(_.languages)
  }

  it must "return the second language if 2 languages are entered but only the second includes a word with a piece using this alphabet (reusing set instances)" in {
    reusingSetInstance(checkReturnLanguageIncludingWordUsingThisAlphabet(nonTested, underTest))
  }
}
