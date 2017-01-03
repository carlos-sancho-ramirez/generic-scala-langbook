package sword.langbook.db.redundant

import sword.db.StorageManager.Key
import sword.db._
import sword.langbook.db.registers.Agent.TargetBunchReferenceField
import sword.langbook.db.registers._

object Text extends RegisterDefinition[Text] {
  object SymbolArrayReferenceField extends NullableSymbolArrayReferenceFieldDefinition {
    override def newField = apply
  }
  case class SymbolArrayReferenceField(override val collectionId: Register.CollectionId) extends AbstractNullableSymbolArrayReferenceField {
    override val definition = SymbolArrayReferenceField
  }

  object CharSequenceField extends CharSequenceFieldDefinition {
    override def newField = apply
  }
  case class CharSequenceField(override val value: String) extends AbstractCharSequenceField {
    override val definition = CharSequenceField
  }

  override val fields = List(
    SymbolArrayReferenceField,
    CharSequenceField
  )

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
  override val fields = List(
    Text.SymbolArrayReferenceField(symbolArray),
    Text.CharSequenceField(text)
  )
}

trait TextReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def newField: Key => AbstractTextReferenceField
  override val target = Text
  override def from(value: String, keyExtractor: String => Option[Key]) = {
    keyExtractor(value).map(newField)
  }
}

trait AbstractTextReferenceField extends ForeignKeyField {
  override def definition: TextReferenceFieldDefinition
}
