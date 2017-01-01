package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

/**
 * Reference to an array of symbols.
 *
 * This contains a numeric identifier that must match with an existing collection.
 */
object SymbolArrayReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  override val target = SymbolPosition
  protected override def from = new SymbolArrayReferenceField(_)
}

case class SymbolArrayReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = SymbolArrayReferenceFieldDefinition
  override def toString = collectionId.toString
}

/**
 * This is a copy of SymbolArrayReferenceFieldDefinition.
 *
 * This copy is required because ConversionPair register uses 2 references to symbol array.
 * This should be removed when possible.
 */
object TargetSymbolArrayReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  override val target = SymbolPosition
  protected override def from = new TargetSymbolArrayReferenceField(_)
}

case class TargetSymbolArrayReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = TargetSymbolArrayReferenceFieldDefinition
  override def toString = collectionId.toString
}

object SymbolPosition extends ArrayableRegisterDefinition[SymbolPosition] {
  override val fields = Vector(SymbolReferenceFieldDefinition)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    keyExtractor(SymbolReferenceFieldDefinition)(values.head).map(SymbolPosition(_))
  }
}

case class SymbolPosition(symbol :StorageManager.Key) extends Register {
  override val definition = SymbolPosition
  override val fields = Vector(SymbolReferenceField(symbol))
}

object NullableSymbolArrayReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  override val target = SymbolPosition
  protected override def from = new NullableSymbolArrayReferenceField(_)
}

case class NullableSymbolArrayReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = NullableSymbolArrayReferenceFieldDefinition
  override def toString = collectionId.toString
}
