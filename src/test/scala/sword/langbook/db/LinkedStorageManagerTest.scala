package sword.langbook.db

import org.scalatest.{FlatSpec, Matchers}
import sword.db.MemoryStorageManager

class LinkedStorageManagerTest extends FlatSpec with Matchers {

  behavior of "LinkedStorageManager"

  it can "be created" in {
    LinkedStorageManager(defs => new MemoryStorageManager(defs))
  }
}
