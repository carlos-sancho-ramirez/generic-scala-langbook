package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

/**
 * Reference to an array of symbols.
 *
 * This contains a numeric identifier that must match with an existing collection.
 */
object ConversionArrayReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  override val target = ConversionPosition
  protected override def from = new ConversionArrayReferenceField(_)
}

case class ConversionArrayReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = ConversionArrayReferenceFieldDefinition
  override def toString = collectionId.toString
}

object ConversionPosition extends ArrayableRegisterDefinition[ConversionPosition] {
  override val fields = Vector(ConversionPairReferenceFieldDefinition)
  override def from(values: Seq[String], keyExtractor: FieldDefinition => String => Option[Key]) = {
    keyExtractor(ConversionPairReferenceFieldDefinition)(values.head).map(ConversionPosition(_))
  }
}

case class ConversionPosition(pair :StorageManager.Key) extends Register {
  override val definition = ConversionPosition
  override val fields = Vector(ConversionPairReferenceField(pair))
}
