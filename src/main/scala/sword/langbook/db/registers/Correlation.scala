package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object Correlation extends CollectibleRegisterDefinition[Correlation] {
  object AlphabetReferenceField extends AlphabetReferenceFieldDefinition {
    override def newField = apply
  }
  case class AlphabetReferenceField(override val key: Key) extends AbstractAlphabetReferenceField {
    override val definition = AlphabetReferenceField
  }

  object SymbolArrayReferenceField extends SymbolArrayReferenceFieldDefinition {
    override def newField = apply
  }
  case class SymbolArrayReferenceField(override val collectionId: Register.CollectionId) extends AbstractSymbolArrayReferenceField {
    override val definition = SymbolArrayReferenceField
  }

  override def fields = Vector(
    AlphabetReferenceField,
    SymbolArrayReferenceField
  )

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        alphabetKey <- keyExtractor(AlphabetReferenceField)(values.head)
        symbolArray <- Register.collectionIdFrom(values(1))
      } yield {
        Correlation(alphabetKey, symbolArray)
      }
    }
    else None
  }
}

case class Correlation(alphabet :StorageManager.Key, symbolArray :Register.CollectionId) extends Register {
  override def definition = Correlation
  override def fields = Vector(
    Correlation.AlphabetReferenceField(alphabet),
    Correlation.SymbolArrayReferenceField(symbolArray)
  )
}

trait NullableCorrelationReferenceFieldDefinition extends NullableCollectionReferenceFieldDefinition {
  override def newField: Register.CollectionId => AbstractNullableCorrelationReferenceField
  override val target = SymbolPosition
}

trait AbstractNullableCorrelationReferenceField extends NullableCollectionReferenceField {
  override def definition: NullableCorrelationReferenceFieldDefinition
  override def toString = collectionId.toString
}
