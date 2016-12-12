package sword.langbook.db

import sword.db.{Register, RegisterDefinition, StorageManager}

case class LinkedStorageManager(storageManagerFactory :(List[RegisterDefinition[Register]]) => StorageManager) {
  val registerDefinitions = List(
      registers.Concept,
      registers.ConceptTypeRelation,
      registers.Alphabet,
      registers.Language,
      registers.Symbol,
      registers.SymbolPosition,
      registers.Word,
      registers.Acceptation,
      registers.WordRepresentation,
      registers.AcceptationRepresentation
  )

  val storageManager = storageManagerFactory(registerDefinitions)

  def concepts = storageManager.getKeysFor(registers.Concept).groupBy(x => x).map { case (key, _) =>
    (key, Concept(key))
  }

  def alphabets = storageManager.getKeysFor(registers.Alphabet).groupBy(x => x).map { case (key, _) =>
    (key, Alphabet(key))
  }

  def languages = storageManager.getKeysFor(registers.Language).groupBy(x => x).map { case (key, _) =>
    (key, Language(key))
  }

  def symbols = storageManager.getKeysFor(registers.Symbol).groupBy(x => x).map { case (key, _) =>
    (key, Symbol(key))
  }

  def words = storageManager.getKeysFor(registers.Word).groupBy(x => x).map { case (key, _) =>
    (key, Word(key))
  }
}
