package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db.{FieldDefinition, Register, RegisterDefinition}

object BunchWord extends RegisterDefinition[BunchWord] {
  object BunchReferenceField extends BunchReferenceFieldDefinition {
    override def newField = apply
  }
  case class BunchReferenceField(override val key: Key) extends AbstractBunchReferenceField {
    override val definition = BunchReferenceField
  }

  object WordReferenceField extends WordReferenceFieldDefinition {
    override def newField = apply
  }
  case class WordReferenceField(override val key: Key) extends AbstractWordReferenceField {
    override val definition = WordReferenceField
  }

  override val fields = Vector(BunchReferenceField, WordReferenceField)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        bunch <- keyExtractor(BunchReferenceField)(values.head)
        word <- keyExtractor(WordReferenceField)(values(1))
      } yield {
        BunchWord(bunch, word)
      }
    }
    else None
  }
}

case class BunchWord(bunch: Key, word: Key) extends Register {
  override val definition = BunchWord
  override val fields = Vector(
    BunchWord.BunchReferenceField(bunch),
    BunchWord.WordReferenceField(word)
  )
}
