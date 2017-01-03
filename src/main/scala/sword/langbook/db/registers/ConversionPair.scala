package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object ConversionPair extends ArrayableRegisterDefinition[ConversionPair] {
  object SourceSymbolArrayReferenceField extends SymbolArrayReferenceFieldDefinition {
    override def newField = apply
  }
  case class SourceSymbolArrayReferenceField(override val collectionId: Register.CollectionId) extends AbstractSymbolArrayReferenceField {
    override val definition = SourceSymbolArrayReferenceField
  }

  object TargetSymbolArrayReferenceField extends SymbolArrayReferenceFieldDefinition {
    override def newField = apply
  }
  case class TargetSymbolArrayReferenceField(override val collectionId: Register.CollectionId) extends AbstractSymbolArrayReferenceField {
    override val definition = TargetSymbolArrayReferenceField
  }

  override def fields = Vector(
    SourceSymbolArrayReferenceField,
    TargetSymbolArrayReferenceField
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
    ConversionPair.SourceSymbolArrayReferenceField(sourceSymbolArray),
    ConversionPair.TargetSymbolArrayReferenceField(targetSymbolArray)
  )
}

trait ConversionArrayReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  override def newField: Register.CollectionId => AbstractConversionArrayReferenceField
  override val target = SymbolPosition
}

trait AbstractConversionArrayReferenceField extends CollectionReferenceField {
  override def definition: ConversionArrayReferenceFieldDefinition
  override def toString = collectionId.toString
}
