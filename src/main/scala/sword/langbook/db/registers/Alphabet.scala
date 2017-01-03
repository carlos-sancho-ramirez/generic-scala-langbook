package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._
import sword.langbook.db.registers.Language.ConceptReferenceField

object Alphabet extends RegisterDefinition[Alphabet] {
  object ConceptReferenceField extends ConceptReferenceFieldDefinition {
    override def newField = apply
  }
  case class ConceptReferenceField(override val key: Key) extends AbstractConceptReferenceField {
    override val definition = ConceptReferenceField
  }

  override val fields = List(ConceptReferenceField)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => (String) => Option[Key]) = {
    if (values.size == fields.size) {
      keyExtractor(ConceptReferenceField)(values.head).map(Alphabet(_))
    }
    else None
  }
}

case class Alphabet(concept :StorageManager.Key) extends Register {
  override val definition = Alphabet
  override val fields = List(Alphabet.ConceptReferenceField(concept))
}

trait AlphabetReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def newField: Key => AbstractAlphabetReferenceField
  override val target = Alphabet
  override def from(value: String, keyExtractor: String => Option[Key]) = {
    keyExtractor(value).map(newField)
  }
}

trait AbstractAlphabetReferenceField extends ForeignKeyField {
  override def definition: AlphabetReferenceFieldDefinition
}
