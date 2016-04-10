package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object PieceReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  override val target = Piece
  protected override def from = new PieceReferenceField(_)
}

case class PieceReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = PieceReferenceFieldDefinition
  override def toString = collectionId.toString
}

object Piece extends CollectibleRegisterDefinition[Piece] {
  override def fields = Vector(
    AlphabetReferenceFieldDefinition,
    SymbolArrayReferenceFieldDefinition)

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      val alphabetKey = keyExtractor(AlphabetReferenceFieldDefinition)(values.head)
      val symbolArray = Register.collectionIdFrom(values(1))
      if (alphabetKey.isDefined && symbolArray.isDefined) {
        Some(Piece(alphabetKey.get, symbolArray.get))
      }
      else None
    }
    else None
  }
}

case class Piece(alphabet :StorageManager.Key, symbolArray :Register.CollectionId) extends Register {
  override def definition = Piece
  override def fields = Vector(
    AlphabetReferenceField(alphabet),
    SymbolArrayReferenceField(symbolArray))
}
