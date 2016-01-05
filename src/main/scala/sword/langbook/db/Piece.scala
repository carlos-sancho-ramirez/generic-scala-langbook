package sword.langbook.db

object PieceReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  def target = PieceCollectionIdentifierFieldDefinition
}

case class PieceReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = PieceReferenceFieldDefinition
  override def toString = collectionId.toString
}

object PieceCollectionIdentifierFieldDefinition extends CollectionIdentifierFieldDefinition

case class PieceCollectionIdentifierField(override val value :Register.CollectionId) extends CollectionIdentifierField {
  override val definition = PieceCollectionIdentifierFieldDefinition
}

object Piece extends RegisterDefinition {
  override val isCollectible = true
  override def fields = Vector(
    PieceCollectionIdentifierFieldDefinition,
    AlphabetReferenceFieldDefinition,
    SymbolArrayReferenceFieldDefinition)
}

case class Piece(collectionId :Register.CollectionId, alphabet :Register.Key, symbolArray :Register.CollectionId) extends Register {
  override def definition = Piece
  override def fields = Vector(
    PieceCollectionIdentifierField(collectionId),
    AlphabetReferenceField(alphabet),
    SymbolArrayReferenceField(symbolArray))
}
