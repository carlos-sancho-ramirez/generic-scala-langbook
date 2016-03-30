package sword.langbook.db

import sword.db.{ForeignKeyField, StorageManager}

case class Alphabet(key :StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def conceptKeyOpt = fields.collectFirst {
    case field :ForeignKeyField if field.definition.target == registers.Concept => field.key
  }

  def concept = Concept(conceptKeyOpt.get)

  // TODO: To be implemented
  def languages = ???

  /**
   * Returns a suitable human readable string for the given language if any
   * @param preferredLanguage Desired language that the user would like to read.
   */
  def suitableTextForLanguage(preferredLanguage: Language): Option[String] = {
    val preferredWordOption = concept.wordsForLanguage(preferredLanguage).headOption
    val wordOption = {
      if (preferredWordOption.isDefined) preferredWordOption
      else concept.words.headOption
    }

    wordOption.flatMap(_.suitableText)
  }
}

object Alphabet extends ElementFactory[registers.Alphabet, Alphabet] {
  def from(manager: LinkedStorageManager, concept: Concept): Option[Alphabet] = {
    from(manager, registers.Alphabet(concept.key))
  }
}
