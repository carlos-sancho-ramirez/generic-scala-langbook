package sword.langbook.db.redundant

import sword.db.{ForeignKeyField, _}
import sword.db.StorageManager.Key
import sword.langbook.db.registers.{AbstractNullableWordReferenceField, NullableWordReferenceFieldDefinition}

object RedundantWord extends RegisterDefinition[RedundantWord] {
  object WordReferenceField extends NullableWordReferenceFieldDefinition {
    override def newField = apply
  }
  case class WordReferenceField(override val key: Key) extends AbstractNullableWordReferenceField {
    override val definition = WordReferenceField
  }

  override val fields = List(WordReferenceField)
  override def from(values: Seq[String],
                    keyExtractor: FieldDefinition => (String) => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        word <- keyExtractor(WordReferenceField)(values.head)
      } yield {
        RedundantWord(word)
      }
    }
    else None
  }
}

case class RedundantWord(word: Key) extends Register {
  override val definition = RedundantWord
  override val fields = List(RedundantWord.WordReferenceField(word))
}

trait RedundantWordReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def newField: Key => AbstractRedundantWordReferenceField
  override val target = RedundantWord
  override def from(value: String, keyExtractor: String => Option[Key]) = {
    keyExtractor(value).map(newField)
  }
}

trait AbstractRedundantWordReferenceField extends ForeignKeyField {
  override def definition: RedundantWordReferenceFieldDefinition
}
