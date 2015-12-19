package sword.langbook.db

import sword.langbook.db.Register.SetId

object PieceReferenceFieldDefinition extends SetReferenceFieldDefinition {
  def target = PieceSetIdentifierFieldDefinition
}

case class PieceReferenceField(override val setId :Register.SetId) extends SetReferenceField {
  override val definition = PieceReferenceFieldDefinition
  override def toString = setId.toString
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
