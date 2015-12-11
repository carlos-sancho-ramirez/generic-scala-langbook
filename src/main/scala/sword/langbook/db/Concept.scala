package sword.langbook.db

/**
 * Definition for fields containing a general-purpose char sequence (string).
 */
object ConceptReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Concept
}

case class ConceptReferenceField(concept :Register.Key) extends Field {
  override val definition = ConceptReferenceFieldDefinition
  override def toString = concept.toString
}

object Concept extends RegisterDefinition {
  override val fields = List(CharSequenceFieldDefinition)
}

case class Concept(hint :String) extends Register {
  override val definition = Concept
  override val fields = List(CharSequenceField(hint))
}