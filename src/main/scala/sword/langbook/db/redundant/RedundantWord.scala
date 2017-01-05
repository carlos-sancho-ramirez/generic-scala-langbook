package sword.langbook.db.redundant

import sword.db.{ForeignKeyField, _}
import sword.db.StorageManager.Key
import sword.langbook.db.registers.{AbstractNullableWordReferenceField, AbstractWordReferenceField, NullableWordReferenceFieldDefinition, WordReferenceFieldDefinition}

object RedundantWord extends RegisterDefinition[RedundantWord] {

  /**
   * Reference to any existing word matching this
   */
  object WordReferenceField extends NullableWordReferenceFieldDefinition {
    override def newField = apply
  }
  case class WordReferenceField(override val key: Key) extends AbstractNullableWordReferenceField {
    override val definition = WordReferenceField
  }

  /**
   * Reference for the original word used to create this one.
   */
  object OriginalWordReferenceField extends WordReferenceFieldDefinition {
    override def newField = apply
  }
  case class OriginalWordReferenceField(override val key: Key) extends AbstractWordReferenceField {
    override val definition = OriginalWordReferenceField
  }

  override val fields = List(
    WordReferenceField,
    OriginalWordReferenceField
  )

  override def from(values: Seq[String],
                    keyExtractor: FieldDefinition => (String) => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        word <- keyExtractor(WordReferenceField)(values.head)
        originalWord <- keyExtractor(OriginalWordReferenceField)(values(1))
      } yield {
        RedundantWord(word, originalWord)
      }
    }
    else None
  }
}

case class RedundantWord(word: Key, originalWord: Key) extends Register {
  override val definition = RedundantWord
  override val fields = List(
    RedundantWord.WordReferenceField(word),
    RedundantWord.OriginalWordReferenceField(originalWord)
  )
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
