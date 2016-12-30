package sword.langbook.db.redundant

import sword.db.StorageManager.Key
import sword.db._
import sword.langbook.db.registers.{SymbolArrayReferenceField, SymbolArrayReferenceFieldDefinition}

object Text extends RegisterDefinition[Text] {
  override val fields = List(SymbolArrayReferenceFieldDefinition, CharSequenceFieldDefinition)
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
  override val fields = List(SymbolArrayReferenceField(symbolArray), CharSequenceField(text))
}
