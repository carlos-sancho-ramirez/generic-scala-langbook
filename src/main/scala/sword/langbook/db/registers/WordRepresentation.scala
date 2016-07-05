package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object WordRepresentation extends CollectibleRegisterDefinition[WordRepresentation] {
  override def fields = Vector(
    WordReferenceFieldDefinition,
    AlphabetReferenceFieldDefinition,
    SymbolArrayReferenceFieldDefinition)

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      val wordKey = keyExtractor(WordReferenceFieldDefinition)(values.head)
      val alphabetKey = keyExtractor(AlphabetReferenceFieldDefinition)(values(1))
      val symbolArray = Register.collectionIdFrom(values(2))
      if (alphabetKey.isDefined && symbolArray.isDefined) {
        Some(WordRepresentation(wordKey.get, alphabetKey.get, symbolArray.get))
      }
      else None
    }
    else None
  }
}

case class WordRepresentation(word: StorageManager.Key, alphabet :StorageManager.Key,
    symbolArray :Register.CollectionId) extends Register {
  override def definition = WordRepresentation
  override def fields = Vector(
    WordReferenceField(word),
    AlphabetReferenceField(alphabet),
    SymbolArrayReferenceField(symbolArray))
}
