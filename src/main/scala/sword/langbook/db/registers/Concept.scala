package sword.langbook.db.registers

import sword.db._

object ConceptReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Concept
}

case class ConceptReferenceField(override val key :Register.Key) extends ForeignKeyField {
  override val definition = ConceptReferenceFieldDefinition
  override def toString = key.toString
}

object Concept extends RegisterDefinition {
  override val fields = List(CharSequenceFieldDefinition)
}

case class Concept(hint :String) extends Register {
  override val definition = Concept
  override val fields = List(CharSequenceField(hint))
}