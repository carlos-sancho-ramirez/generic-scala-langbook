package sword.langbook.db

object SymbolSequence extends RegisterDefinition { 
  override val fields = Vector(AlphabetReferenceFieldDefinition, SymbolArrayReferenceFieldDefinition)
}

case class SymbolSequence(alphabet :Register.Key, symbolArray :Register.Key) extends Register {
  override val definition = SymbolSequence
  override val fields = Vector(AlphabetReferenceField(alphabet), SymbolArrayReferenceField(symbolArray))
}