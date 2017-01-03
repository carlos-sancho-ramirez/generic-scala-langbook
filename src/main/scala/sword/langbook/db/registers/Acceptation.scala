package sword.langbook.db.registers

import sword.db._
import sword.db.StorageManager.Key

object Acceptation extends RegisterDefinition[Acceptation] {
  object WordReferenceField extends WordReferenceFieldDefinition {
    override def newField = apply
  }
  case class WordReferenceField(override val key: Key) extends AbstractWordReferenceField {
    override val definition = WordReferenceField
  }

  object ConceptReferenceField extends ConceptReferenceFieldDefinition {
    override def newField = apply
  }

  case class ConceptReferenceField(override val key: Key) extends AbstractConceptReferenceField {
    override val definition = ConceptReferenceField
  }

  override val fields = Vector(
    WordReferenceField,
    ConceptReferenceField
  )

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        wordKey <- keyExtractor(WordReferenceField)(values.head)
        conceptKey <- keyExtractor(ConceptReferenceField)(values(1))
      } yield {
        Acceptation(wordKey, conceptKey)
      }
    }
    else None
  }
}

case class Acceptation(word: Key, concept: Key) extends Register {
  override val definition = Acceptation
  override val fields = Vector(Acceptation.WordReferenceField(word), Acceptation.ConceptReferenceField(concept))
}

trait AcceptationReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def newField: Key => AbstractAcceptationReferenceField
  override val target = Acceptation
  override def from(value: String, keyExtractor: String => Option[Key]) = {
    keyExtractor(value).map(newField)
  }
}

trait AbstractAcceptationReferenceField extends ForeignKeyField {
  override def definition: AcceptationReferenceFieldDefinition
}
