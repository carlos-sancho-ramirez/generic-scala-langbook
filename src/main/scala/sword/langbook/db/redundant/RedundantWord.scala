package sword.langbook.db.redundant

import sword.db.{ForeignKeyField, _}
import sword.db.StorageManager.Key
import sword.langbook.db.registers.{NullableWordReferenceField, NullableWordReferenceFieldDefinition}

object RedundantWord extends RegisterDefinition[RedundantWord] {
  override val fields = List(NullableWordReferenceFieldDefinition)
  override def from(values: Seq[String],
                    keyExtractor: FieldDefinition => (String) => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        word <- keyExtractor(NullableWordReferenceFieldDefinition)(values.head)
      } yield {
        RedundantWord(word)
      }
    }
    else None
  }
}

case class RedundantWord(word: Key) extends Register {
  override val definition = RedundantWord
  override val fields = List(NullableWordReferenceField(word))
}

object RedundantWordReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  override val target = RedundantWord
  override def from(value: String, keyExtractor: String => Option[Key]) = {
    keyExtractor(value).map(RedundantWordReferenceField)
  }
}

case class RedundantWordReferenceField(override val key: Key) extends ForeignKeyField {
  override val definition = RedundantWordReferenceFieldDefinition
  override def toString = key.toString
}
