package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object Conversion extends RegisterDefinition[Conversion] {
  override def fields = Vector(
    AlphabetReferenceFieldDefinition,
    TargetAlphabetReferenceFieldDefinition,
    ConversionArrayReferenceFieldDefinition
  )

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        sourceAlphabetKey <- keyExtractor(AlphabetReferenceFieldDefinition)(values.head)
        targetAlphabetKey <- keyExtractor(TargetAlphabetReferenceFieldDefinition)(values(1))
        conversionArray <- Register.collectionIdFrom(values(2))
      } yield {
        Conversion(sourceAlphabetKey, targetAlphabetKey, conversionArray)
      }
    }
    else None
  }
}

case class Conversion(sourceAlphabet: StorageManager.Key, targetAlphabet: StorageManager.Key,
                      conversionArray :Register.CollectionId) extends Register {
  override def definition = Conversion
  override def fields = Vector(
    AlphabetReferenceField(sourceAlphabet),
    TargetAlphabetReferenceField(targetAlphabet),
    ConversionArrayReferenceField(conversionArray))
}
