package sword.langbook.db

object PieceReferenceFieldDefinition extends SetReferenceFieldDefinition {
  def target = Piece
}

case class PieceReferenceField(piece :Register.Key) extends Field {
  override val definition = PieceReferenceFieldDefinition
  override def toString = piece.toString
}

object Piece extends RegisterDefinition {
  override def fields = Vector(SetIdentifierFieldDefinition, AlphabetReferenceFieldDefinition,
    SymbolArrayReferenceFieldDefinition)
}

case class Piece(setId :Register.SetId, alphabet :Register.Key, symbolArray :Register.Key) extends Register {
  override def definition = Piece
  override def fields = Vector(SetIdentifierField(setId), AlphabetReferenceField(alphabet),
    SymbolArrayReferenceField(symbolArray))
}
