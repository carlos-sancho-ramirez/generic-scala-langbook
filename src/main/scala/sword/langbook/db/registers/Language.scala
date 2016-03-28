package sword.langbook.db.registers

import sword.db._

object LanguageReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Language
}

case class LanguageReferenceField(override val key: StorageManager.Key) extends ForeignKeyField {
  override val definition = LanguageReferenceFieldDefinition
  override def toString = key.toString
}

object Language extends RegisterDefinition {
  override val fields = List(
    ConceptReferenceFieldDefinition,
    LanguageCodeFieldDefinition,
    AlphabetReferenceFieldDefinition)
}

case class Language(concept: StorageManager.Key, code: Register.LanguageCode,
    preferredAlphabet: StorageManager.Key) extends Register {
  override val definition = Language
  override val fields = List(
    ConceptReferenceField(concept),
    LanguageCodeField(code),
    AlphabetReferenceField(preferredAlphabet))
}
