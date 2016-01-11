package sword.langbook.db.registers

import sword.db.{ArrayableRegisterDefinition, CollectionReferenceField, CollectionReferenceFieldDefinition, Register}

object PieceArrayReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  val target = PiecePosition
}

case class PieceArrayReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = PieceArrayReferenceFieldDefinition
  override def toString = collectionId.toString
}

object PiecePosition extends ArrayableRegisterDefinition {
  override val fields = Vector(PieceReferenceFieldDefinition)
}

case class PiecePosition(piece :Register.CollectionId) extends Register {
  override val definition = PiecePosition
  override val fields = Vector(PieceReferenceField(piece))
}