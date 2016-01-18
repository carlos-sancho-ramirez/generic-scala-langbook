package sword.langbook.db.registers

import sword.db.{StorageManager, Register, RegisterDefinition}

object WordConcept extends RegisterDefinition {
  override val fields = Vector(WordReferenceFieldDefinition, ConceptReferenceFieldDefinition)
}

case class WordConcept(word :StorageManager.Key, concept :StorageManager.Key) extends Register {
  override val definition = WordConcept
  override val fields = Vector(WordReferenceField(word), ConceptReferenceField(concept))
}
