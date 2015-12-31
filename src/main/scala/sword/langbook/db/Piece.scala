package sword.langbook.db

import sword.langbook.db.Register.CollectionId

object PieceReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  def target = PieceCollectionIdentifierFieldDefinition$
}

case class PieceReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = PieceReferenceFieldDefinition
  override def toString = collectionId.toString
}

object PieceCollectionIdentifierFieldDefinition$ extends CollectionIdentifierFieldDefinition

object Piece extends RegisterDefinition {
  override def fields = Vector(PieceCollectionIdentifierFieldDefinition$, AlphabetReferenceFieldDefinition,
    SymbolArrayReferenceFieldDefinition)
}

case class Piece(setId :Register.CollectionId, alphabet :Register.Key, symbolArray :Register.Key) extends Register {
  override def definition = Piece
  override def fields = Vector(CollectionIdentifierField(PieceCollectionIdentifierFieldDefinition$, setId), AlphabetReferenceField(alphabet),
    SymbolArrayReferenceField(symbolArray))
}
