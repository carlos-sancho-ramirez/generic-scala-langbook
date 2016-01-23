package sword.langbook.db

import sword.db.{CollectionReferenceField, ForeignKeyField, StorageManager}

case class Piece(key :StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def alphabetKeyOpt = fields.collectFirst {
    case field :ForeignKeyField if field.definition.target == registers.Alphabet => field.key
  }
  def symbolArrayIdOpt = fields.collectFirst {
    case field :CollectionReferenceField if field.definition.target == registers.SymbolPosition => field.collectionId
  }

  def alphabet = Alphabet(alphabetKeyOpt.get)
  def symbols = SymbolArray(key.storageManager, symbolArrayIdOpt.get)
}
