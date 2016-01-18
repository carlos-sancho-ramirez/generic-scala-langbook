package sword.langbook.db.registers

import sword.db._

object WordReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Word
}

case class WordReferenceField(override val key :StorageManager.Key) extends ForeignKeyField {
  override val definition = WordReferenceFieldDefinition
  override def toString = key.toString
}

object Word extends RegisterDefinition {
  override val fields = Vector(LanguageReferenceFieldDefinition, PieceArrayReferenceFieldDefinition)
}

case class Word(language :StorageManager.Key, pieceArray :Register.CollectionId) extends Register {
  override val definition = Word
  override val fields = Vector(LanguageReferenceField(language), PieceArrayReferenceField(pieceArray))
}
