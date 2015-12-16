package sword.langbook.db

object WordReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Word
}

case class WordReferenceField(word :Register.Key) extends Field {
  override val definition = WordReferenceFieldDefinition
  override def toString = word.toString
}

object Word extends RegisterDefinition {
  override val fields = Vector(LanguageReferenceFieldDefinition, PieceArrayReferenceFieldDefinition)
}

case class Word(language :Register.Key, pieceArray :Register.Key) extends Register {
  override val definition = Word
  override val fields = Vector(LanguageReferenceField(language), PieceArrayReferenceField(pieceArray))
}