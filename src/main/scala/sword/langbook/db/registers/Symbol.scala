package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object SymbolReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  override val target = Symbol
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(SymbolReferenceField)
  }
}

case class SymbolReferenceField(override val key :StorageManager.Key) extends ForeignKeyField {
  override val definition = SymbolReferenceFieldDefinition
  override def toString = key.toString
}

object Symbol extends RegisterDefinition[Symbol] {
  override val fields = List(UnicodeFieldDefinition, CharSequenceFieldDefinition)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => (String) => Option[Key]) = {
    if (values.size <= fields.size) Register.unicodeTypeFrom(values.head).map(Symbol(_))
    else None
  }
}

case class Symbol(unicode :Register.UnicodeType) extends Register {
  override val definition = Symbol
  override val fields = List(UnicodeField(unicode), CharSequenceField("" + unicode.toChar))
}
