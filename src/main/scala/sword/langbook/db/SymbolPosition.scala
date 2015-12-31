package sword.langbook.db

/**
 * Reference to an array of symbols.
 *
 * This contains a numeric identifier that must match with an existing collection.
 */
object SymbolArrayReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  override val target = SymbolArrayIdentifierFieldDefinition
}

case class SymbolArrayReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = SymbolArrayReferenceFieldDefinition
  override def toString = collectionId.toString
}

object SymbolArrayIdentifierFieldDefinition extends CollectionIdentifierFieldDefinition

case class SymbolArrayIdentifierField(override val value :Register.CollectionId) extends CollectionIdentifierField {
  override val definition = SymbolArrayIdentifierFieldDefinition
}

object SymbolArrayIndexFieldDefinition extends ArrayIndexFieldDefinition {
  override val collection = SymbolArrayIdentifierFieldDefinition
}

case class SymbolArrayIndexField(override val index :Register.Index) extends ArrayIndexField {
  override val definition = SymbolArrayIndexFieldDefinition
}

object SymbolPosition extends RegisterDefinition {
  override val fields = Vector(SymbolReferenceFieldDefinition, SymbolArrayIdentifierFieldDefinition,
      SymbolArrayIndexFieldDefinition)
}

case class SymbolPosition(symbol :Register.Key, arrayId :Register.CollectionId,
    index :Register.Index) extends Register {
  override val definition = SymbolPosition
  override val fields = Vector(SymbolReferenceField(symbol),
      SymbolArrayIdentifierField(arrayId),
      SymbolArrayIndexField(index))
}