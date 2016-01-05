package sword.langbook.db

object PieceArrayReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  val target = PiecePosition
}

case class PieceArrayReferenceField(override val key :Register.Key) extends ForeignKeyField {
  override val definition = PieceArrayReferenceFieldDefinition
  override def toString = key.toString
}

object PieceArrayIdentifierFieldDefinition extends CollectionIdentifierFieldDefinition

case class PieceArrayIdentifierField(override val value :Register.CollectionId) extends CollectionIdentifierField {
  override val definition = PieceArrayIdentifierFieldDefinition
}

object PieceArrayIndexFieldDefinition extends ArrayIndexFieldDefinition {
  override val collection = PieceArrayIdentifierFieldDefinition
}

case class PieceArrayIndexField(override val index :Register.Index) extends ArrayIndexField {
  override val definition = PieceArrayIndexFieldDefinition
}

object PiecePosition extends RegisterDefinition {
  override val fields = Vector(PieceReferenceFieldDefinition,
    PieceArrayIdentifierFieldDefinition,
    PieceArrayIndexFieldDefinition)
}

case class PiecePosition(piece :Register.Key, arrayId :Register.CollectionId, index :Register.Index) extends Register {
  override val definition = PiecePosition
  override val fields = Vector(PieceReferenceField(arrayId),
    PieceArrayIdentifierField(arrayId),
    PieceArrayIndexField(index))
}