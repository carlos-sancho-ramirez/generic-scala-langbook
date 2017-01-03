package sword.langbook.db.registers

import sword.db._
import sword.db.StorageManager.Key

object Concept extends RegisterDefinition[Concept] {
  object CharSequenceField extends CharSequenceFieldDefinition {
    override def newField = apply
  }
  case class CharSequenceField(override val value: String) extends AbstractCharSequenceField {
    override val definition = CharSequenceField
  }

  override val fields = List(CharSequenceField)
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
  override val fields = List(Concept.CharSequenceField(hint))
}

trait ConceptReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def newField: Key => AbstractConceptReferenceField
  override val target = Concept
  override def from(value: String, keyExtractor: String => Option[Key]) = {
    keyExtractor(value).map(newField)
  }
}

trait AbstractConceptReferenceField extends ForeignKeyField {
  override def definition: ConceptReferenceFieldDefinition
}
