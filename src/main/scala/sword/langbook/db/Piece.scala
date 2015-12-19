package sword.langbook.db

object PieceReferenceFieldDefinition extends SetReferenceFieldDefinition {
  def target = PieceSetIdentifierFieldDefinition
}

case class PieceReferenceField(piece :Register.Key) extends Field {
  override val definition = PieceReferenceFieldDefinition
  override def toString = piece.toString
}

object PieceSetIdentifierFieldDefinition extends SetIdentifierFieldDefinition

object Piece extends RegisterDefinition {
  override def fields = Vector(PieceSetIdentifierFieldDefinition, AlphabetReferenceFieldDefinition,
    SymbolArrayReferenceFieldDefinition)
}

case class Piece(setId :Register.SetId, alphabet :Register.Key, symbolArray :Register.Key) extends Register {
  override def definition = Piece
  override def fields = Vector(SetIdentifierField(PieceSetIdentifierFieldDefinition, setId), AlphabetReferenceField(alphabet),
    SymbolArrayReferenceField(symbolArray))
}
