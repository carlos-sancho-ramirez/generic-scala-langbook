package sword.langbook.db

object Piece extends RegisterDefinition {
  override def fields = Vector(SetIdentifierFieldDefinition, AlphabetReferenceFieldDefinition,
    SymbolArrayReferenceFieldDefinition)
}

case class Piece(setId :Register.SetId, alphabet :Register.Key, symbolArray :Register.Key) extends Register {
  override def definition = Piece
  override def fields = Vector(SetIdentifierField(setId), AlphabetReferenceField(alphabet),
    SymbolArrayReferenceField(symbolArray))
}
