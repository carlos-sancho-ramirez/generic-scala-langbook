package sword.langbook.db.registers

import sword.db.{FieldDefinition, Register, RegisterDefinition}
import sword.db.StorageManager.Key

object WordRepresentation extends RegisterDefinition[WordRepresentation] {
  object WordReferenceField extends WordReferenceFieldDefinition {
    override def newField = apply
  }
  case class WordReferenceField(override val key: Key) extends AbstractWordReferenceField {
    override val definition = WordReferenceField
  }

  object AlphabetReferenceField extends AlphabetReferenceFieldDefinition {
    override def newField = apply
  }
  case class AlphabetReferenceField(override val key: Key) extends AbstractAlphabetReferenceField {
    override val definition = AlphabetReferenceField
  }

  object SymbolArrayReferenceField extends SymbolArrayReferenceFieldDefinition {
    override def newField = apply
  }
  case class SymbolArrayReferenceField(override val collectionId: Register.CollectionId) extends AbstractSymbolArrayReferenceField {
    override val definition = SymbolArrayReferenceField
  }

  override def fields = Vector(
    WordReferenceField,
    AlphabetReferenceField,
    SymbolArrayReferenceField
  )

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        wordKey <- keyExtractor(WordReferenceField)(values.head)
        alphabetKey <- keyExtractor(AlphabetReferenceField)(values(1))
        symbolArray <- Register.collectionIdFrom(values(2))
      } yield {
        WordRepresentation(wordKey, alphabetKey, symbolArray)
      }
    }
    else None
  }
}

case class WordRepresentation(word: Key, alphabet: Key,
    symbolArray: Register.CollectionId) extends Register {
  override def definition = WordRepresentation
  override def fields = Vector(
    WordRepresentation.WordReferenceField(word),
    WordRepresentation.AlphabetReferenceField(alphabet),
    WordRepresentation.SymbolArrayReferenceField(symbolArray)
  )
}
