package sword.langbook.db

import sword.db.{CollectionReferenceField, ForeignKeyField, StorageManager}

case class Word(key :StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def languageKeyOpt = fields.collectFirst {
    case field :ForeignKeyField if field.definition.target == registers.Language => field.key
  }
  def pieceArrayIdOpt = fields.collectFirst {
    case field :CollectionReferenceField if field.definition.target == registers.PiecePosition => field.collectionId
  }

  def language = Language(languageKeyOpt.get)
  def pieces = PieceArray(key.storageManager, pieceArrayIdOpt.get)
}
