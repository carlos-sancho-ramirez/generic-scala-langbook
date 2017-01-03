package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object Symbol extends RegisterDefinition[Symbol] {
  override val fields = List(UnicodeFieldDefinition)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => (String) => Option[Key]) = {
    if (values.size <= fields.size) Register.unicodeTypeFrom(values.head).map(Symbol(_))
    else None
  }
}

case class Symbol(unicode :Register.UnicodeType) extends Register {
  override val definition = Symbol
  override val fields = List(UnicodeField(unicode))
}

trait SymbolReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def newField: Key => AbstractSymbolReferenceField
  override val target = Symbol
  override def from(value: String, keyExtractor: String => Option[Key]) = {
    keyExtractor(value).map(newField)
  }
}

trait AbstractSymbolReferenceField extends ForeignKeyField {
  override def definition: SymbolReferenceFieldDefinition
}
