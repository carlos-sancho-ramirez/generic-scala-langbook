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

    val conceptOption = manager.addConcept(ConceptParams("MyConcept"))
    conceptOption shouldBe defined

    val concepts = manager.concepts
    concepts.size shouldBe 1
    concepts.values.head shouldBe conceptOption.get
  }

  it can "insert a language and its concept at once" in {
    val manager = newManager
    manager.languages shouldBe empty

    val conceptHint = "MyLanguageConcept"
    val languageOption = manager.addLanguage(LanguageParams(ConceptParams(conceptHint)))
    languageOption shouldBe defined

    val concepts = manager.concepts
    concepts.size shouldBe 1
    concepts.values.head.hint shouldBe conceptHint

    val languages = manager.languages
    languages.size shouldBe 1
    languages.values.head shouldBe languageOption.get
  }
}
