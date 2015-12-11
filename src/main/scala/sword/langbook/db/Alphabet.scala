package sword.langbook.db

object Alphabet extends RegisterDefinition {
  override val fields = List(ConceptReferenceFieldDefinition)
}

case class Alphabet(concept :Register.Key) extends Register {
  override val definition = Alphabet
  override val fields = List(ConceptReferenceField(concept))
}
