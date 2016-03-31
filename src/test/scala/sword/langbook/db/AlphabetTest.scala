package sword.langbook.db

import org.scalatest.{FlatSpec, Matchers}
import sword.db.MemoryStorageManager

class AlphabetTest extends FlatSpec with Matchers {

  private def newManager = {
    LinkedStorageManager(defs => new MemoryStorageManager(defs))
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
}
