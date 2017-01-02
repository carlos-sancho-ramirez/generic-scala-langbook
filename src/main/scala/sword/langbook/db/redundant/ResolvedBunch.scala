package sword.langbook.db.redundant

import sword.db.{FieldDefinition, Register, RegisterDefinition}
import sword.db.StorageManager.Key
import sword.langbook.db.registers.{BunchReferenceField, BunchReferenceFieldDefinition}

object ResolvedBunch extends RegisterDefinition[ResolvedBunch] {
  override val fields = Vector(BunchReferenceFieldDefinition, RedundantWordReferenceFieldDefinition)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        bunch <- keyExtractor(BunchReferenceFieldDefinition)(values.head)
        word <- keyExtractor(RedundantWordReferenceFieldDefinition)(values(1))
      } yield {
        ResolvedBunch(bunch, word)
      }
    }
    else None
  }
}

case class ResolvedBunch(bunch: Key, word: Key) extends Register {
  override val definition = ResolvedBunch
  override val fields = Vector(BunchReferenceField(bunch), RedundantWordReferenceField(word))
}
