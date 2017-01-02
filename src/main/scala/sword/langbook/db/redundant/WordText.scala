package sword.langbook.db.redundant

import sword.db.StorageManager.Key
import sword.db.{FieldDefinition, Register, RegisterDefinition, StorageManager}
import sword.langbook.db.registers.{AlphabetReferenceField, AlphabetReferenceFieldDefinition}

object WordText extends RegisterDefinition[WordText] {
  override def fields = Vector(
    RedundantWordReferenceFieldDefinition,
    AlphabetReferenceFieldDefinition,
    TextReferenceFieldDefinition
  )

  override def from(values: Seq[String],
                    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        word <- keyExtractor(RedundantWordReferenceFieldDefinition)(values.head)
        alphabet <- keyExtractor(AlphabetReferenceFieldDefinition)(values(1))
        text <- keyExtractor(TextReferenceFieldDefinition)(values(2))
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
    RedundantWordReferenceField(redundantWord),
    AlphabetReferenceField(alphabet),
    TextReferenceField(text)
  )
}
