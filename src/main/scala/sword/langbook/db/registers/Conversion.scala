package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._
import sword.langbook.db.registers.Alphabet.ConceptReferenceField
import sword.langbook.db.registers.WordRepresentation.SymbolArrayReferenceField

object Conversion extends RegisterDefinition[Conversion] {
  object SourceAlphabetReferenceField extends AlphabetReferenceFieldDefinition {
    override def newField = apply
  }
  case class SourceAlphabetReferenceField(override val key: Key) extends AbstractAlphabetReferenceField {
    override val definition = SourceAlphabetReferenceField
  }

  object TargetAlphabetReferenceField extends AlphabetReferenceFieldDefinition {
    override def newField = apply
  }
  case class TargetAlphabetReferenceField(override val key: Key) extends AbstractAlphabetReferenceField {
    override val definition = TargetAlphabetReferenceField
  }

  object ConversionArrayReferenceField extends ConversionArrayReferenceFieldDefinition {
    override def newField = apply
  }
  case class ConversionArrayReferenceField(override val collectionId: Register.CollectionId) extends AbstractConversionArrayReferenceField {
    override val definition = ConversionArrayReferenceField
  }

  override def fields = Vector(
    SourceAlphabetReferenceField,
    TargetAlphabetReferenceField,
    ConversionArrayReferenceField
  )

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        sourceAlphabetKey <- keyExtractor(SourceAlphabetReferenceField)(values.head)
        targetAlphabetKey <- keyExtractor(TargetAlphabetReferenceField)(values(1))
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
    Conversion.SourceAlphabetReferenceField(sourceAlphabet),
    Conversion.TargetAlphabetReferenceField(targetAlphabet),
    Conversion.ConversionArrayReferenceField(conversionArray))
}
