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
