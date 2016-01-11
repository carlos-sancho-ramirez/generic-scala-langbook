package sword.langbook.db

import sword.db.{RegisterDefinition, StorageManager}

case class LinkedStorageManager(storageManagerFactory :(List[RegisterDefinition]) => StorageManager) {
  val registerDefinitions = List(registers.Concept, registers.Alphabet, registers.Language)
  val storageManager = storageManagerFactory(registerDefinitions)

  def concepts = storageManager.getMapFor(registers.Concept).collect { case (k, v :registers.Concept) => (k,v)}
  def alphabets = storageManager.getKeysFor(registers.Alphabet).groupBy(x => x).map { case (key, _) =>
    (key, Alphabet(storageManager, key))
  }
  def languages = storageManager.getKeysFor(registers.Language).groupBy(x => x).map { case (key, _) =>
    (key, Language(storageManager, key))
  }
}
