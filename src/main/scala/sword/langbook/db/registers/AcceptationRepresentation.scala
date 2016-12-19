package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object AcceptationRepresentation extends RegisterDefinition[AcceptationRepresentation] {
  override def fields = Vector(
    AcceptationReferenceFieldDefinition,
    SymbolArrayReferenceFieldDefinition)

  override def from(values: Seq[String],
                    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      val acceptationKey = keyExtractor(AcceptationReferenceFieldDefinition)(values.head)
      val symbolArray = Register.collectionIdFrom(values(1))
      if (symbolArray.isDefined) {
        Some(AcceptationRepresentation(acceptationKey.get, symbolArray.get))
      }
      else None
    }
    else None
  }
}

// Currently only used with Japanese to relate a Kanji representation with its meaning.
// So representation can be assumed to be in Kanji alphabet.
case class AcceptationRepresentation(acceptation: StorageManager.Key,
    symbolArray :Register.CollectionId) extends Register {

  override def definition = AcceptationRepresentation
  override def fields = Vector(
    AcceptationReferenceField(acceptation),
    SymbolArrayReferenceField(symbolArray))
}
