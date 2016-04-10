package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object AlphabetReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  override val target = Alphabet
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(AlphabetReferenceField)
  }
}

case class AlphabetReferenceField(override val key :StorageManager.Key) extends ForeignKeyField {
  override val definition = AlphabetReferenceFieldDefinition
  override def toString = key.toString
}

object Alphabet extends RegisterDefinition[Alphabet] {
  override val fields = List(ConceptReferenceFieldDefinition)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => (String) => Option[Key]) = {
    if (values.size == fields.size) {
      keyExtractor(ConceptReferenceFieldDefinition)(values.head).map(Alphabet(_))
    }
    else None
  }
}

case class Alphabet(concept :StorageManager.Key) extends Register {
  override val definition = Alphabet
  override val fields = List(ConceptReferenceField(concept))
}
