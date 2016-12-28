package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

/**
 * Reference to a conversion, which is an array of conversion pairs.
 */
object ConversionArrayReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  override val target = ConversionPair
  protected override def from = new ConversionArrayReferenceField(_)
}

case class ConversionArrayReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = ConversionArrayReferenceFieldDefinition
  override def toString = collectionId.toString
}

object ConversionPair extends ArrayableRegisterDefinition[ConversionPair] {
  override def fields = Vector(
    SymbolArrayReferenceFieldDefinition,
    TargetSymbolArrayReferenceFieldDefinition
  )

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        sourceSymbolArray <- Register.collectionIdFrom(values.head)
        targetSymbolArray <- Register.collectionIdFrom(values(1))
      } yield {
        ConversionPair(sourceSymbolArray, targetSymbolArray)
      }
    }
    else None
  }
}

case class ConversionPair(sourceSymbolArray: Register.CollectionId, targetSymbolArray :Register.CollectionId) extends Register {
  override def definition = ConversionPair
  override def fields = Vector(
    SymbolArrayReferenceField(sourceSymbolArray),
    TargetSymbolArrayReferenceField(targetSymbolArray)
  )
}
