package sword.langbook.db

import sword.db.{RegisterDefinition, StorageManager}

case class LinkedStorageManager(storageManagerFactory :(List[RegisterDefinition]) => StorageManager) {
  val registerDefinitions = List(
      registers.Concept,
      registers.Alphabet,
      registers.Language,
      registers.LanguageAlphabet,
      registers.Piece,
      registers.PiecePosition,
      registers.Symbol,
      registers.SymbolPosition,
      registers.Word,
      registers.WordConcept)

  val storageManager = storageManagerFactory(registerDefinitions)

  def concepts = storageManager.getMapFor(registers.Concept).collect { case (k, v :registers.Concept) => (k,v)}
  def alphabets = storageManager.getKeysFor(registers.Alphabet).groupBy(x => x).map { case (key, _) =>
    (key, Alphabet(key))
  }
  def languages = storageManager.getKeysFor(registers.Language).groupBy(x => x).map { case (key, _) =>
    (key, Language(key))
  }

  def words = storageManager.getKeysFor(registers.Word).groupBy(x => x).map { case (key, _) =>
    (key, Word(key))
  }
}
