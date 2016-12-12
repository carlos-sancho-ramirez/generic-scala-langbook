package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object AcceptationReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  override val target = Acceptation
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(AcceptationReferenceField)
  }
}

case class AcceptationReferenceField(override val key :StorageManager.Key) extends ForeignKeyField {
  override val definition = AcceptationReferenceFieldDefinition
  override def toString = key.toString
}

object Acceptation extends RegisterDefinition[Acceptation] {
  override val fields = Vector(WordReferenceFieldDefinition, ConceptReferenceFieldDefinition)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      val wordKey = keyExtractor(WordReferenceFieldDefinition)(values.head)
      val conceptKey = keyExtractor(ConceptReferenceFieldDefinition)(values(1))
      if (wordKey.isDefined && conceptKey.isDefined) {
        Some(Acceptation(wordKey.get, conceptKey.get))
      }
      else None
    }
    else None
  }
}

case class Acceptation(word :StorageManager.Key, concept :StorageManager.Key) extends Register {
  override val definition = Acceptation
  override val fields = Vector(WordReferenceField(word), ConceptReferenceField(concept))
}
