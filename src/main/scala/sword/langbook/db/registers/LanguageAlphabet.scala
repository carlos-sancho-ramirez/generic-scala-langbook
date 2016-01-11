package sword.langbook.db.registers

import sword.db.{Register, RegisterDefinition}

object LanguageAlphabet extends RegisterDefinition {
  override val fields = Vector(LanguageReferenceFieldDefinition, AlphabetReferenceFieldDefinition)
}

case class LanguageAlphabet(language :Register.Key, alphabet :Register.Key) extends Register {
  override val definition = LanguageAlphabet
  override val fields = Vector(LanguageReferenceField(language), AlphabetReferenceField(alphabet))
}