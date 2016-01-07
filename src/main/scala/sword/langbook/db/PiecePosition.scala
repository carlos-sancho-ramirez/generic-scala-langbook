package sword.langbook.db

object PieceArrayReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  val target = PiecePosition
}

case class PieceArrayReferenceField(override val key :Register.Key) extends ForeignKeyField {
  override val definition = PieceArrayReferenceFieldDefinition
  override def toString = key.toString
}

object PiecePosition extends ArrayableRegisterDefinition {
  override val fields = Vector(PieceReferenceFieldDefinition)
}

case class PiecePosition(piece :Register.CollectionId) extends Register {
  override val definition = PiecePosition
  override val fields = Vector(PieceReferenceField(piece))
}