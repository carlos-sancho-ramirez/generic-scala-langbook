package sword.langbook.db.registers

import sword.db.{StorageManager, Register, RegisterDefinition}

object LanguageAlphabet extends RegisterDefinition {
  override val fields = Vector(LanguageReferenceFieldDefinition, AlphabetReferenceFieldDefinition)
}

case class LanguageAlphabet(language :StorageManager.Key, alphabet :StorageManager.Key) extends Register {
  override val definition = LanguageAlphabet
  override val fields = Vector(LanguageReferenceField(language), AlphabetReferenceField(alphabet))
}