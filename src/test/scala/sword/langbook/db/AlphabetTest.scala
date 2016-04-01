package sword.langbook.db

import org.scalatest.{FlatSpec, Matchers}
import sword.db.MemoryStorageManager

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

      set(testedAlphabet).size shouldBe targetLanguages.size
      set(testedAlphabet).diff(targetLanguages) shouldBe empty

      for (targetLanguage <- targetLanguages) {
        set(testedAlphabet).contains(targetLanguage) shouldBe true
      }
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
}
