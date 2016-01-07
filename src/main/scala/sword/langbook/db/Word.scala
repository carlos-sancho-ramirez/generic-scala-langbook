package sword.langbook.db

object WordReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Word
}

case class WordReferenceField(override val key :Register.Key) extends ForeignKeyField {
  override val definition = WordReferenceFieldDefinition
  override def toString = key.toString
}

object Word extends RegisterDefinition {
  override val fields = Vector(LanguageReferenceFieldDefinition, PieceArrayReferenceFieldDefinition)
}

case class Word(language :Register.Key, pieceArray :Register.CollectionId) extends Register {
  override val definition = Word
  override val fields = Vector(LanguageReferenceField(language), PieceArrayReferenceField(pieceArray))
}
