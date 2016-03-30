package sword.langbook.db

import sword.db._

import scala.collection.Set

case class Language(key :StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def conceptKeyOpt = fields.collectFirst {
    case field :ForeignKeyField if field.definition.target == registers.Concept => field.key
  }
  def preferredAlphabetKeyOpt = fields.collectFirst {
    case field :ForeignKeyField if field.definition.target == registers.Alphabet => field.key
  }

  def concept = Concept(conceptKeyOpt.get)

  /**
   * Returns the ISO 639-1 2 lower-case char string that uniquely identifies this language
   */
  def code = fields.collectFirst {
    case field :LanguageCodeField => field.code
  }.get

  /**
   * Returns a human readable string for this language.
   * This first looks for a word written in this same language. If not, it just pick the first word
   * representing this in any language it is written.
   *
   * @return A some instance with the text inside, or None if no word was found.
   */
  def suitableText: Option[String] = {
    val firstOption = concept.wordsForLanguage(this).headOption
    val wordOption = if (firstOption.isEmpty) concept.words.headOption else firstOption
    wordOption.flatMap(_.suitableText)
  }

  /**
   * Retrieves a suitable human readable string based in the given preferredLanguage.
   * If there is no word assigned for the given language, it will look for alternatives.
   * @param preferredLanguage Desired language the user what to read
   * @return A some instance containing a string, or None if no possible text can be found.
   */
  def suitableTextForLanguage(preferredLanguage: Language): Option[String] = {
    val firstOption = concept.wordsForLanguage(preferredLanguage).headOption
    if (firstOption.isEmpty) suitableText else firstOption.flatMap(_.suitableText)
  }

  def preferredAlphabet = Alphabet(preferredAlphabetKeyOpt.get)

  lazy val alphabets = new scala.collection.AbstractSet[Alphabet]() {
    private def retrieveInnerSet = {
      val allWords = key.storageManager.getMapFor(registers.Word).values.filter {
        _.fields.collectFirst {
          case field :ForeignKeyField if field.definition.target == registers.Language && field.key == key =>
            true
        }.isDefined
      }

      val pieceArrays = allWords.flatMap(_.fields.collectFirst {
        case field :CollectionReferenceField if field.definition.target == registers.PiecePosition =>
          field.collectionId
      })

      val piecePositions = pieceArrays.flatMap(key.storageManager.getKeysForArray(registers.PiecePosition,_))
        .flatMap(key.storageManager.get)

      val pieces = piecePositions.flatMap(_.fields.collectFirst{
        case field: CollectionReferenceField if field.definition.target == registers.Piece =>
          field.collectionId
      }).flatMap(key.storageManager.getKeysForCollection(registers.Piece,_)).flatMap(key.storageManager.get)

      pieces.flatMap(_.fields.collectFirst {
        case field: ForeignKeyField if field.definition.target == registers.Alphabet =>
          field.key
      }).toSet[StorageManager.Key].map(key => Alphabet(key)) + preferredAlphabet
    }

    override def contains(elem: Alphabet) = retrieveInnerSet.contains(elem)

    override def +(elem: Alphabet): Set[Alphabet] = ???

    override def -(elem: Alphabet): Set[Alphabet] = ???

    override def iterator = retrieveInnerSet.iterator
  }

  override def equals(other: Any) = {
    other.isInstanceOf[Language] && key == other.asInstanceOf[Language].key
  }
}

object Language extends ElementFactory[registers.Language, Language] {
  def from(manager: LinkedStorageManager, concept: Concept, code: Register.LanguageCode,
      preferredAlphabet: Alphabet): Option[Language] = {
    from(manager, registers.Language(concept.key, code, preferredAlphabet.key))
  }
}
