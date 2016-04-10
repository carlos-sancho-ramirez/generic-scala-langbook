package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

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

object Word extends RegisterDefinition[Word] {
  override val fields = Vector(LanguageReferenceFieldDefinition, PieceArrayReferenceFieldDefinition)
  override def from(values: Seq[String],
      keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      val languageKey = keyExtractor(LanguageReferenceFieldDefinition)(values.head)
      val pieceArray = Register.collectionIdFrom(values(1))
      if (languageKey.isDefined && pieceArray.isDefined) {
        Some(Word(languageKey.get, pieceArray.get))
      }
      else None
    }
    else None
  }
}

case class Word(language :StorageManager.Key, pieceArray :Register.CollectionId) extends Register {
  override val definition = Word
  override val fields = Vector(LanguageReferenceField(language), PieceArrayReferenceField(pieceArray))
}
