package sword.langbook.db.registers

import sword.db.{FieldDefinition, Register, RegisterDefinition}
import sword.db.StorageManager.Key

object AcceptationRepresentation extends RegisterDefinition[AcceptationRepresentation] {
  object AcceptationReferenceField extends AcceptationReferenceFieldDefinition {
    override def newField = apply
  }
  case class AcceptationReferenceField(override val key: Key) extends AbstractAcceptationReferenceField {
    override val definition = AcceptationReferenceField
  }

  object SymbolArrayReferenceField extends SymbolArrayReferenceFieldDefinition {
    override def newField = apply
  }
  case class SymbolArrayReferenceField(override val collectionId: Register.CollectionId) extends AbstractSymbolArrayReferenceField {
    override val definition = SymbolArrayReferenceField
  }

  override def fields = Vector(
    AcceptationReferenceField,
    SymbolArrayReferenceField
  )

  override def from(values: Seq[String],
                    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        acceptationKey <- keyExtractor(AcceptationReferenceField)(values.head)
        symbolArray <- Register.collectionIdFrom(values(1))
      } yield {
        AcceptationRepresentation(acceptationKey, symbolArray)
      }
    }
    else None
  }
}

// Currently only used with Japanese to relate a Kanji representation with its meaning.
// So representation can be assumed to be in Kanji alphabet.
case class AcceptationRepresentation(acceptation: Key,
    symbolArray: Register.CollectionId) extends Register {

  override def definition = AcceptationRepresentation
  override def fields = Vector(
    AcceptationRepresentation.AcceptationReferenceField(acceptation),
    AcceptationRepresentation.SymbolArrayReferenceField(symbolArray)
  )
}
