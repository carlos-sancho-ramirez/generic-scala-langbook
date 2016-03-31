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

  private def checkReturnLanguageWithThisAlphabetAsPreferred(set: Alphabet => scala.collection.Set[Language]): Unit = {
    val manager = newManager
    val alphabet = Alphabet.from(manager, Concept.from(manager, "Alphabet").get).get
    val languageConcept = Concept.from(manager, "Language").get
    set(alphabet) shouldBe empty

    val language = Language.from(manager, languageConcept, "xx", alphabet).get
    set(alphabet).size shouldBe 1
    set(alphabet).head shouldBe language
  }

  private def checkReturnAllLanguagesWithThisAlphabetAsPreferred(set: Alphabet => scala.collection.Set[Language]): Unit = {
    val manager = newManager
    val alphabet = Alphabet.from(manager, Concept.from(manager, "Alphabet").get).get
    set(alphabet) shouldBe empty

    val language = Language.from(manager, Concept.from(manager, "Language").get, "xx", alphabet).get
    val language2 = Language.from(manager, Concept.from(manager, "Language2").get, "xy", alphabet).get
    set(alphabet).size shouldBe 2
    set(alphabet).contains(language) shouldBe true
    set(alphabet).contains(language2) shouldBe true
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

  it must "return the language that uses this alphabet as preferred" in {
    checkReturnLanguageWithThisAlphabetAsPreferred(_.languages)
  }

  it must "return the language that uses this alphabet as preferred (reusing set instances)" in {
    reusingSetInstance(checkReturnLanguageWithThisAlphabetAsPreferred)
  }

  it must "return all languages that uses this alphabet as preferred" in {
    checkReturnAllLanguagesWithThisAlphabetAsPreferred(_.languages)
  }

  it must "return all languages that uses this alphabet as preferred (reusing set instances)" in {
    reusingSetInstance(checkReturnAllLanguagesWithThisAlphabetAsPreferred)
  }
}
