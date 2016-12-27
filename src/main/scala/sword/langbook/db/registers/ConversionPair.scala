package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object ConversionPairReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  override val target = ConversionPair
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(ConversionPairReferenceField)
  }
}

case class ConversionPairReferenceField(override val key :StorageManager.Key) extends ForeignKeyField {
  override val definition = ConversionPairReferenceFieldDefinition
  override def toString = key.toString
}

object ConversionPair extends RegisterDefinition[ConversionPair] {
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
