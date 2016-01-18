package sword.langbook.db.registers

import sword.db._

object AlphabetReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Alphabet
}

case class AlphabetReferenceField(override val key :StorageManager.Key) extends ForeignKeyField {
  override val definition = AlphabetReferenceFieldDefinition
  override def toString = key.toString
}

object Alphabet extends RegisterDefinition {
  override val fields = List(ConceptReferenceFieldDefinition)
}

case class Alphabet(concept :StorageManager.Key) extends Register {
  override val definition = Alphabet
  override val fields = List(ConceptReferenceField(concept))
}
