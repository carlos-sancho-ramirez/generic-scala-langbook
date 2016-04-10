package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object PieceArrayReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  val target = PiecePosition
  protected override def from = new PieceArrayReferenceField(_)
}

case class PieceArrayReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = PieceArrayReferenceFieldDefinition
  override def toString = collectionId.toString
}

object PiecePosition extends ArrayableRegisterDefinition[PiecePosition] {
  override val fields = Vector(PieceReferenceFieldDefinition)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) Register.collectionIdFrom(values.head).map(PiecePosition(_))
    else None
  }
}

case class PiecePosition(piece :Register.CollectionId) extends Register {
  override val definition = PiecePosition
  override val fields = Vector(PieceReferenceField(piece))
}