package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object WordConcept extends RegisterDefinition[WordConcept] {
  override val fields = Vector(WordReferenceFieldDefinition, ConceptReferenceFieldDefinition)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      val wordKey = keyExtractor(WordReferenceFieldDefinition)(values.head)
      val conceptKey = keyExtractor(ConceptReferenceFieldDefinition)(values(1))
      if (wordKey.isDefined && conceptKey.isDefined) {
        Some(WordConcept(wordKey.get, conceptKey.get))
      }
      else None
    }
    else None
  }
}

case class WordConcept(word :StorageManager.Key, concept :StorageManager.Key) extends Register {
  override val definition = WordConcept
  override val fields = Vector(WordReferenceField(word), ConceptReferenceField(concept))
}
