package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._
import sword.langbook.db.registers.Language.ConceptReferenceField

object SymbolPosition extends ArrayableRegisterDefinition[SymbolPosition] {
  object SymbolReferenceField extends SymbolReferenceFieldDefinition {
    override def newField = apply
  }
  case class SymbolReferenceField(override val key: Key) extends AbstractSymbolReferenceField {
    override val definition = SymbolReferenceField
  }

  override val fields = Vector(SymbolReferenceField)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    keyExtractor(SymbolReferenceField)(values.head).map(SymbolPosition(_))
  }
}

case class SymbolPosition(symbol :StorageManager.Key) extends Register {
  override val definition = SymbolPosition
  override val fields = Vector(SymbolPosition.SymbolReferenceField(symbol))
}

trait SymbolArrayReferenceFieldDefinition extends CollectionReferenceFieldDefinition {
  override def newField: Register.CollectionId => AbstractSymbolArrayReferenceField
  override val target = SymbolPosition
}

trait AbstractSymbolArrayReferenceField extends CollectionReferenceField {
  override def definition: SymbolArrayReferenceFieldDefinition
  override def toString = collectionId.toString
}

trait NullableSymbolArrayReferenceFieldDefinition extends NullableCollectionReferenceFieldDefinition {
  override def newField: Register.CollectionId => AbstractNullableSymbolArrayReferenceField
  override val target = SymbolPosition
}

trait AbstractNullableSymbolArrayReferenceField extends NullableCollectionReferenceField {
  override def definition: NullableSymbolArrayReferenceFieldDefinition
  override def toString = collectionId.toString
}
