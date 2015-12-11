package sword.langbook.db

object LanguageReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Language
}

case class LanguageReferenceField(language :Register.Key) extends Field {
  override val definition = LanguageReferenceFieldDefinition
  override def toString = language.toString
}

object Language extends RegisterDefinition {
  override val fields = List(ConceptReferenceFieldDefinition)
}

case class Language(concept :Register.Key) extends Register {
  override val definition = Language
  override val fields = List(ConceptReferenceField(concept))
}
