package sword.langbook.db

object PieceArrayReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  val target = PiecePosition
}

case class PieceArrayReferenceField(piecePosition :Register.Key) extends Field {
  override val definition = PieceArrayReferenceFieldDefinition
  override def toString = piecePosition.toString
}

object PiecePosition extends RegisterDefinition {
  override val fields = Vector(PieceReferenceFieldDefinition, ArrayIndexFieldDefinition)
}

case class PiecePosition(piece :Register.Key, index :Register.Index) extends Register {
  override val definition = PiecePosition
  override val fields = Vector(PieceReferenceField(piece), ArrayIndexField(index))
}