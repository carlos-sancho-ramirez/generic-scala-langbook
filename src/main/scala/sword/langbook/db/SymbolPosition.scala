package sword.langbook.db

/**
 * Reference to an array of symbols.
 *
 * This contains a numeric identifier that must match with an existing collection.
 */
object SymbolArrayReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  override val target = Symbol
}

case class SymbolArrayReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = SymbolArrayReferenceFieldDefinition
  override def toString = collectionId.toString
}

object SymbolPosition extends RegisterDefinition {
  override val isCollectible = true
  override val isArrayable = true
  override val fields = Vector(SymbolReferenceFieldDefinition)
}

case class SymbolPosition(symbol :Register.Key) extends Register {
  override val definition = SymbolPosition
  override val fields = Vector(SymbolReferenceField(symbol))
}
