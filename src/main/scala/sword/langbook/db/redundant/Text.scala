package sword.langbook.db.redundant

import sword.db.StorageManager.Key
import sword.db._
import sword.langbook.db.registers.{NullableSymbolArrayReferenceField, NullableSymbolArrayReferenceFieldDefinition, SymbolArrayReferenceField, SymbolArrayReferenceFieldDefinition}

object Text extends RegisterDefinition[Text] {
  override val fields = List(NullableSymbolArrayReferenceFieldDefinition, CharSequenceFieldDefinition)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => (String) => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        symbolArrayId <- Register.collectionIdFrom(values.head)
      } yield {
        Text(symbolArrayId, values(1))
      }
    }
    else None
  }
}

case class Text(symbolArray: Register.CollectionId, text: String) extends Register {
  override val definition = Text
  override val fields = List(NullableSymbolArrayReferenceField(symbolArray), CharSequenceField(text))
}

object TextReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  override val target = Text
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(TextReferenceField)
  }
}

case class TextReferenceField(override val key :StorageManager.Key) extends ForeignKeyField {
  override val definition = TextReferenceFieldDefinition
  override def toString = key.toString
}
