package sword.langbook.db

import org.scalatest.{FlatSpec, Matchers}
import sword.db.MemoryStorageManager

class LinkedStorageManagerTest extends FlatSpec with Matchers {

  behavior of "LinkedStorageManager"

  it can "be created" in {
    LinkedStorageManager(defs => new MemoryStorageManager(defs))
  }

  it can "insert a concept" in {
    val manager = LinkedStorageManager(defs => new MemoryStorageManager(defs))
    manager.concepts shouldBe empty

    val conceptOption = manager.addConcept(ConceptParams("MyConcept"))
    conceptOption shouldBe defined

    val concepts = manager.concepts
    concepts.size shouldBe 1
    concepts.values.head shouldBe conceptOption.get
  }
}
