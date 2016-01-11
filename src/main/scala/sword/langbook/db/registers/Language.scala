package sword.langbook.db.registers

import sword.db.{ForeignKeyField, ForeignKeyFieldDefinition, Register, RegisterDefinition}

object LanguageReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Language
}

case class LanguageReferenceField(override val key :Register.Key) extends ForeignKeyField {
  override val definition = LanguageReferenceFieldDefinition
  override def toString = key.toString
}

object Language extends RegisterDefinition {
  override val fields = List(ConceptReferenceFieldDefinition)
}

case class Language(concept :Register.Key) extends Register {
  override val definition = Language
  override val fields = List(ConceptReferenceField(concept))
}
