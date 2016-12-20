package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object Correlation extends CollectibleRegisterDefinition[Correlation] {
  override def fields = Vector(
    AlphabetReferenceFieldDefinition,
    SymbolArrayReferenceFieldDefinition)

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      val alphabetKey = keyExtractor(AlphabetReferenceFieldDefinition)(values.head)
      val symbolArray = Register.collectionIdFrom(values(1))
      if (alphabetKey.isDefined && symbolArray.isDefined) {
        Some(Correlation(alphabetKey.get, symbolArray.get))
      }
      else None
    }
    else None
  }
}

case class Correlation(alphabet :StorageManager.Key, symbolArray :Register.CollectionId) extends Register {
  override def definition = Correlation
  override def fields = Vector(
    AlphabetReferenceField(alphabet),
    SymbolArrayReferenceField(symbolArray))
}

object CorrelationReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  override val target = Correlation
  protected override def from = new CorrelationReferenceField(_)
}

case class CorrelationReferenceField(override val collectionId :Register.CollectionId) extends CollectionReferenceField {
  override val definition = CorrelationReferenceFieldDefinition
  override def toString = collectionId.toString
}
