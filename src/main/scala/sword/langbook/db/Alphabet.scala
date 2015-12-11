package sword.langbook.db

object AlphabetReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Alphabet
}

case class AlphabetReferenceField(alphabet :Register.Key) extends Field {
  override val definition = AlphabetReferenceFieldDefinition
  override def toString = alphabet.toString
}

object Alphabet extends RegisterDefinition {
  override val fields = List(ConceptReferenceFieldDefinition)
}

case class Alphabet(concept :Register.Key) extends Register {
  override val definition = Alphabet
  override val fields = List(ConceptReferenceField(concept))
}
