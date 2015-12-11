package sword.langbook.db

object SymbolReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Symbol
}

case class SymbolReferenceField(symbol :Register.Key) extends Field {
  override val definition = ConceptReferenceFieldDefinition
  override def toString = symbol.toString
}

object Symbol extends RegisterDefinition { 
  override val fields = List(UnicodeFieldDefinition)
}

case class Symbol(unicode :Register.UnicodeType) extends Register {
  override val definition = Symbol
  override val fields = List(UnicodeField(unicode))
}