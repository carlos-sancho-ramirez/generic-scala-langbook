package sword.langbook.db

object Symbol extends RegisterDefinition { 
  override val fields = List(UnicodeFieldDefinition)
}

case class Symbol(unicode :Register.UnicodeType) extends Register {
  override val definition = Symbol
  override val fields = List(UnicodeField(unicode))
}