package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object LanguageReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  override val target = Language
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(LanguageReferenceField)
  }
}

case class LanguageReferenceField(override val key: StorageManager.Key) extends ForeignKeyField {
  override val definition = LanguageReferenceFieldDefinition
  override def toString = key.toString
}

object Language extends RegisterDefinition[Language] {
  override val fields = List(
    ConceptReferenceFieldDefinition,
    LanguageCodeFieldDefinition,
    AlphabetReferenceFieldDefinition)

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => (String) => Option[Key]) = {
    if (values.size == fields.size) {
      val conceptKey = keyExtractor(ConceptReferenceFieldDefinition)(values.head)
      val preferredAlphabet = keyExtractor(AlphabetReferenceFieldDefinition)(values(2))
      if (conceptKey.isDefined && preferredAlphabet.isDefined) {
        Some(Language(conceptKey.get, values(1), preferredAlphabet.get))
      }
      else None
    }
    else None
  }
}

case class Language(concept: StorageManager.Key, code: Register.LanguageCode,
    preferredAlphabet: StorageManager.Key) extends Register {
  override val definition = Language
  override val fields = List(
    ConceptReferenceField(concept),
    LanguageCodeField(code),
    AlphabetReferenceField(preferredAlphabet))
}
