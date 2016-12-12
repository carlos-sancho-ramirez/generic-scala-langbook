package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db.{CollectibleRegisterDefinition, FieldDefinition, Register, StorageManager}

object AcceptationRepresentation extends CollectibleRegisterDefinition[AcceptationRepresentation] {
  override def fields = Vector(
    AcceptationReferenceFieldDefinition,
    AlphabetReferenceFieldDefinition,
    SymbolArrayReferenceFieldDefinition)

  override def from(values: Seq[String],
                    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      val acceptationKey = keyExtractor(AcceptationReferenceFieldDefinition)(values.head)
      val alphabetKey = keyExtractor(AlphabetReferenceFieldDefinition)(values(1))
      val symbolArray = Register.collectionIdFrom(values(2))
      if (alphabetKey.isDefined && symbolArray.isDefined) {
        Some(AcceptationRepresentation(acceptationKey.get, alphabetKey.get, symbolArray.get))
      }
      else None
    }
    else None
  }
}

case class AcceptationRepresentation(acceptation: StorageManager.Key,
    alphabet :StorageManager.Key, symbolArray :Register.CollectionId) extends Register {

  override def definition = AcceptationRepresentation
  override def fields = Vector(
    AcceptationReferenceField(acceptation),
    AlphabetReferenceField(alphabet),
    SymbolArrayReferenceField(symbolArray))
}
