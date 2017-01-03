package sword.langbook.db.redundant

import sword.db.StorageManager.Key
import sword.db.{FieldDefinition, Register, RegisterDefinition, StorageManager}
import sword.langbook.db.registers.{AbstractAlphabetReferenceField, AlphabetReferenceFieldDefinition}

object WordText extends RegisterDefinition[WordText] {
  object AlphabetReferenceField extends AlphabetReferenceFieldDefinition {
    override def newField = apply
  }
  case class AlphabetReferenceField(override val key: Key) extends AbstractAlphabetReferenceField {
    override val definition = AlphabetReferenceField
  }

  object RedundantWordReferenceField extends RedundantWordReferenceFieldDefinition {
    override def newField = apply
  }
  case class RedundantWordReferenceField(override val key: Key) extends AbstractRedundantWordReferenceField {
    override val definition = RedundantWordReferenceField
  }

  object TextReferenceField extends TextReferenceFieldDefinition {
    override def newField = apply
  }
  case class TextReferenceField(override val key: Key) extends AbstractTextReferenceField {
    override val definition = TextReferenceField
  }

  override def fields = Vector(
    RedundantWordReferenceField,
    AlphabetReferenceField,
    TextReferenceField
  )

  override def from(values: Seq[String],
                    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        word <- keyExtractor(RedundantWordReferenceField)(values.head)
        alphabet <- keyExtractor(AlphabetReferenceField)(values(1))
        text <- keyExtractor(TextReferenceField)(values(2))
      } yield {
        WordText(word, alphabet, text)
      }
    }
    else None
  }
}

case class WordText(redundantWord: StorageManager.Key, alphabet :StorageManager.Key,
                    text: StorageManager.Key) extends Register {
  override def definition = WordText
  override def fields = Vector(
    WordText.RedundantWordReferenceField(redundantWord),
    WordText.AlphabetReferenceField(alphabet),
    WordText.TextReferenceField(text)
  )
}
