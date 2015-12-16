package sword.langbook.db

class MemoryStorageManagerTest extends StorageManagerTest {
  override def newStorageManager(registerDefinitions: Seq[RegisterDefinition]) =
      new MemoryStorageManager(registerDefinitions)
}
