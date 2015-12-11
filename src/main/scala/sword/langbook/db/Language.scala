package sword.langbook.db

object Language extends RegisterDefinition {
  override val fields = List(ConceptReferenceFieldDefinition)
}

case class Language(concept :Register.Key) extends Register {
  override val definition = Language
  override val fields = List(ConceptReferenceField(concept))
}
