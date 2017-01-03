package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._
import sword.langbook.db.registers.Alphabet.ConceptReferenceField

object Word extends RegisterDefinition[Word] {
  object LanguageReferenceField extends LanguageReferenceFieldDefinition {
    override def newField = apply
  }
  case class LanguageReferenceField(override val key: Key) extends AbstractLanguageReferenceField {
    override val definition = LanguageReferenceField
  }

  override val fields = Vector(LanguageReferenceField)
  override def from(values: Seq[String],
      keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      keyExtractor(LanguageReferenceField)(values.head).map(key => Word(key))
    }
    else None
  }
}

case class Word(language :StorageManager.Key) extends Register {
  override val definition = Word
  override val fields = Vector(Word.LanguageReferenceField(language))
}

trait WordReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def newField: Key => AbstractWordReferenceField
  override val target = Word
  override def from(value: String, keyExtractor: String => Option[Key]) = {
    keyExtractor(value).map(newField)
  }
}

trait AbstractWordReferenceField extends ForeignKeyField {
  override def definition: WordReferenceFieldDefinition
}

trait NullableWordReferenceFieldDefinition extends NullableForeignKeyFieldDefinition {
  def newField: Key => AbstractNullableWordReferenceField
  override val target = Word
  override def from(value: String, keyExtractor: String => Option[Key]) = {
    keyExtractor(value).map(newField)
  }
}

trait AbstractNullableWordReferenceField extends NullableForeignKeyField {
  override def definition: NullableWordReferenceFieldDefinition
}
