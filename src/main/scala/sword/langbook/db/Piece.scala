package sword.langbook.db

object PieceReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  override val target = Piece
}

case class PieceReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = PieceReferenceFieldDefinition
  override def toString = collectionId.toString
}

object Piece extends RegisterDefinition {
  override val isCollectible = true
  override def fields = Vector(
    AlphabetReferenceFieldDefinition,
    SymbolArrayReferenceFieldDefinition)
}

case class Piece(alphabet :Register.Key, symbolArray :Register.CollectionId) extends Register {
  override def definition = Piece
  override def fields = Vector(
    AlphabetReferenceField(alphabet),
    SymbolArrayReferenceField(symbolArray))
}
