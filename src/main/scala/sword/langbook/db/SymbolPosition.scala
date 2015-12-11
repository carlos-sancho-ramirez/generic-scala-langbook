package sword.langbook.db

/**
 * Reference to an array of symbols.
 *
 * This contains a foreign key that must match with a SymbolPosition primary key.
 * As this is a reference to an array, it is understood that even if this only
 * point to a single position, all position with an index higher than the one
 * in the position selected will be included in the given order, creating an array. 
 */
object SymbolArrayReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  val target = SymbolPosition
}

case class SymbolArrayReferenceField(symbolPosition :Register.Key) extends Field {
  override val definition = SymbolArrayReferenceFieldDefinition
  override def toString = symbolPosition.toString
}

object SymbolPosition extends RegisterDefinition { 
  override val fields = Vector(SymbolReferenceFieldDefinition, ArrayIndexFieldDefinition)
}

case class SymbolPosition(symbol :Register.Key, index :Register.Index) extends Register {
  override val definition = SymbolPosition
  override val fields = Vector(SymbolReferenceField(symbol), ArrayIndexField(index))
}