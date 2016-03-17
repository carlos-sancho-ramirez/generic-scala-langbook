package sword.langbook.db

import sword.db.{RegisterDefinition, StorageManager}

case class LinkedStorageManager(storageManagerFactory :(List[RegisterDefinition]) => StorageManager) {
  val registerDefinitions = List(
      registers.Concept,
      registers.Alphabet,
      registers.Language,
      registers.Piece,
      registers.PiecePosition,
      registers.Symbol,
      registers.SymbolPosition,
      registers.Word,
      registers.WordConcept)

  val storageManager = storageManagerFactory(registerDefinitions)

  def concepts = storageManager.getKeysFor(registers.Concept).groupBy(x => x).map { case (key, _) =>
    (key, Concept(key))
  }

  def addConcept(params: ConceptParams) = {
    storageManager.insert(registers.Concept(params.hint)).map(Concept)
  }

  def alphabets = storageManager.getKeysFor(registers.Alphabet).groupBy(x => x).map { case (key, _) =>
    (key, Alphabet(key))
  }

  def languages = storageManager.getKeysFor(registers.Language).groupBy(x => x).map { case (key, _) =>
    (key, Language(key))
  }

  def addLanguage(params: LanguageParams) = {
    val conceptKeyOpt = params.concept match {
      case concept: ConceptParams =>
        storageManager.insert(registers.Concept(concept.hint))

      case concept: Concept =>
        Some(concept.key)
    }

    conceptKeyOpt.flatMap(conceptKey => storageManager.insert(registers.Language(conceptKey))).map(Language)
  }

  def words = storageManager.getKeysFor(registers.Word).groupBy(x => x).map { case (key, _) =>
    (key, Word(key))
  }
}
