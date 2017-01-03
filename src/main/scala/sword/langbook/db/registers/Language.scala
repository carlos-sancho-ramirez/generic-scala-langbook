package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object Language extends RegisterDefinition[Language] {
  object ConceptReferenceField extends ConceptReferenceFieldDefinition {
    override def newField = apply
  }
  case class ConceptReferenceField(override val key: Key) extends AbstractConceptReferenceField {
    override val definition = ConceptReferenceField
  }

  object AlphabetReferenceField extends AlphabetReferenceFieldDefinition {
    override def newField = apply
  }
  case class AlphabetReferenceField(override val key: Key) extends AbstractAlphabetReferenceField {
    override val definition = AlphabetReferenceField
  }

  /**
   * CharSequenceField where it is expected to find strings to identify the language "es", "en", "ja"...
   */
  object LanguageCodeField extends CharSequenceFieldDefinition {
    override def newField = apply
  }
  case class LanguageCodeField(override val value: String) extends AbstractCharSequenceField {
    override val definition = LanguageCodeField
  }

  override val fields = List(
    ConceptReferenceField,
    LanguageCodeField,
    AlphabetReferenceField
  )

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => (String) => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        conceptKey <- keyExtractor(ConceptReferenceField)(values.head)
        preferredAlphabet <- keyExtractor(AlphabetReferenceField)(values(2))
      } yield {
        Language(conceptKey, values(1), preferredAlphabet)
      }
    }
    else None
  }
}

case class Language(concept: StorageManager.Key, code: Register.LanguageCode,
    preferredAlphabet: StorageManager.Key) extends Register {
  override val definition = Language
  override val fields = List(
    Language.ConceptReferenceField(concept),
    Language.LanguageCodeField(code),
    Language.AlphabetReferenceField(preferredAlphabet))
}

trait LanguageReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def newField: Key => AbstractLanguageReferenceField
  override val target = Language
  override def from(value: String, keyExtractor: String => Option[Key]) = {
    keyExtractor(value).map(newField)
  }
}

trait AbstractLanguageReferenceField extends ForeignKeyField {
  override def definition: LanguageReferenceFieldDefinition
}
