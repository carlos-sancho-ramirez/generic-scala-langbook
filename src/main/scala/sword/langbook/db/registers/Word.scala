package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object Word extends RegisterDefinition[Word] {
  override val fields = Vector(LanguageReferenceFieldDefinition)
  override def from(values: Seq[String],
      keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      val languageKey = keyExtractor(LanguageReferenceFieldDefinition)(values.head)
      if (languageKey.isDefined) {
        Some(Word(languageKey.get))
      }
      else None
    }
    else None
  }
}

case class Word(language :StorageManager.Key) extends Register {
  override val definition = Word
  override val fields = Vector(LanguageReferenceField(language))
}

object WordReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  override val target = Word
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(WordReferenceField)
  }
}

case class WordReferenceField(override val key :StorageManager.Key) extends ForeignKeyField {
  override val definition = WordReferenceFieldDefinition
  override def toString = key.toString
}

object NullableWordReferenceFieldDefinition extends NullableForeignKeyFieldDefinition {
  override val target = Word
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(NullableWordReferenceField)
  }
}

case class NullableWordReferenceField(override val key: StorageManager.Key) extends NullableForeignKeyField {
  override val definition = NullableWordReferenceFieldDefinition
  override def toString = key.toString
}
