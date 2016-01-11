package sword.langbook.db.registers

import sword.db._

object SymbolReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Symbol
}

case class SymbolReferenceField(override val key :Register.Key) extends ForeignKeyField {
  override val definition = ConceptReferenceFieldDefinition
  override def toString = key.toString
}

object Symbol extends RegisterDefinition { 
  override val fields = List(UnicodeFieldDefinition)
}

case class Symbol(unicode :Register.UnicodeType) extends Register {
  override val definition = Symbol
  override val fields = List(UnicodeField(unicode))
}