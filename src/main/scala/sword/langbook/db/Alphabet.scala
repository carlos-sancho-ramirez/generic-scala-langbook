package sword.langbook.db

import sword.db.StorageManager

case class Alphabet(key :StorageManager.Key) {
  def conceptKeyOpt = key.registerOption.map(_.asInstanceOf[registers.Alphabet].concept)
  def concept = Concept(conceptKeyOpt.get)

  lazy val languages = new scala.collection.AbstractSet[Language] {

    private def wrappedSetForPreferred = key.storageManager.getMapFor(registers.Language)
        .filter(_._2.preferredAlphabet == key).map(t => Language(t._1)).toSet

    private def includedInWords = {
      val words = (for {
        wordKey <- key.storageManager.getMapFor(registers.WordRepresentation, registers.WordRepresentation.AlphabetReferenceField(key)).values.map(_.word)
        word <- key.storageManager.get(wordKey)
      } yield {
        word.asInstanceOf[registers.Word]
      }).toSet

      words.map(word => Language(word.language))
    }

    private def wrappedSet = wrappedSetForPreferred ++ includedInWords

    override def contains(elem: Language) = wrappedSet.contains(elem)
    override def +(elem: Language) = wrappedSet + elem
    override def -(elem: Language) = wrappedSet - elem
    override def iterator = wrappedSet.iterator
  }

  /**
   * Returns a suitable human readable string for the given language if any
   * @param preferredLanguage Desired language that the user would like to read.
   */
  def suitableTextForLanguage(preferredLanguage: Language): Option[String] = {
    concept.wordForLanguageOrFirst(preferredLanguage).flatMap(_.suitableText)
  }

  def symbols = key.storageManager.allSymbolsInAlphabet(key).map(Symbol(_))
}

object Alphabet extends ElementFactory[registers.Alphabet, Alphabet] {
  def from(manager: LinkedStorageManager, concept: Concept): Option[Alphabet] = {
    from(manager, registers.Alphabet(concept.key))
  }
}
