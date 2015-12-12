package sword.langbook.db

object WordConcept extends RegisterDefinition {
  override val fields = Vector(WordReferenceFieldDefinition, ConceptReferenceFieldDefinition)
}

case class WordConcept(word :Register.Key, concept :Register.Key) extends Register {
  override val definition = WordConcept
  override val fields = Vector(WordReferenceField(word), ConceptReferenceField(concept))
}
