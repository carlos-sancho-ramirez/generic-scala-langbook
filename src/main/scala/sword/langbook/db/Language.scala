package sword.langbook.db

import sword.db.{CollectionReferenceField, ForeignKeyField, StorageManager}

sealed trait LanguageEntity
sealed case class Language(key :StorageManager.Key) extends LanguageEntity {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def conceptKeyOpt = fields.collectFirst {
    case field :ForeignKeyField if field.definition.target == registers.Concept => field.key
  }

  def concept = Concept(conceptKeyOpt.get)

  def alphabets = {
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
    }).toSet.map(Alphabet)
  }

  override def equals(other: Any) = {
    other.isInstanceOf[Language] && key == other.asInstanceOf[Language].key
  }
}

sealed case class LanguageParams(concept: ConceptEntity)