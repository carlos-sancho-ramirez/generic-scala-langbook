package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object ConceptReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  override val target = Concept
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(ConceptReferenceField)
  }
}

case class ConceptReferenceField(override val key :StorageManager.Key) extends ForeignKeyField {
  override val definition = ConceptReferenceFieldDefinition
  override def toString = key.toString
}

object Concept extends RegisterDefinition[Concept] {
  override val fields = List(CharSequenceFieldDefinition)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => (String) => Option[Key]) = {
    if (values.size == fields.size) {
      Some(Concept(values.head))
    }
    else None
  }
}

case class Concept(hint :String) extends Register {
  override val definition = Concept
  override val fields = List(CharSequenceField(hint))
}