package sword.langbook.db.redundant

import sword.db.{FieldDefinition, Register, RegisterDefinition}
import sword.db.StorageManager.Key
import sword.langbook.db.registers.WordRepresentation.WordReferenceField
import sword.langbook.db.registers.{AbstractNullableBunchReferenceField, AbstractWordReferenceField, NullableBunchReferenceFieldDefinition, WordReferenceFieldDefinition}

object ResolvedBunch extends RegisterDefinition[ResolvedBunch] {
  object BunchReferenceField extends NullableBunchReferenceFieldDefinition {
    override def newField = apply
  }
  case class BunchReferenceField(override val key: Key) extends AbstractNullableBunchReferenceField {
    override val definition = BunchReferenceField
  }

  object RedundantWordReferenceField extends RedundantWordReferenceFieldDefinition {
    override def newField = apply
  }
  case class RedundantWordReferenceField(override val key: Key) extends AbstractRedundantWordReferenceField {
    override val definition = RedundantWordReferenceField
  }

  override val fields = Vector(BunchReferenceField, RedundantWordReferenceField)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        bunch <- keyExtractor(BunchReferenceField)(values.head)
        word <- keyExtractor(RedundantWordReferenceField)(values(1))
      } yield {
        ResolvedBunch(bunch, word)
      }
    }
    else None
  }
}

case class ResolvedBunch(bunch: Key, word: Key) extends Register {
  override val definition = ResolvedBunch
  override val fields = Vector(
    ResolvedBunch.BunchReferenceField(bunch),
    ResolvedBunch.RedundantWordReferenceField(word)
  )
}
