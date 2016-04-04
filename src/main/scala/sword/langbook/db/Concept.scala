package sword.langbook.db

import sword.db.{ForeignKeyField, CharSequenceField, StorageManager}
import sword.langbook.db.registers.ConceptReferenceField

case class Concept(key: StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def hintOpt = fields.collectFirst {
    case field :CharSequenceField => field.value
  }

  def hint = hintOpt.get

  def words = {
    key.storageManager.getMapFor(registers.WordConcept, ConceptReferenceField(key)).flatMap {
      case (_, reg) =>
        reg.fields.collectFirst {
          case field: ForeignKeyField if field.definition.target == registers.Word =>
            Word(field.key)
        }
    }
  }

  def wordsForLanguage(language :Language) = {
    words.filter(_.language == language)
  }

  def types = {
    key.storageManager.getMapFor(registers.ConceptTypeRelation).flatMap {
      case (_, reg) =>
        if (reg.fields(1).asInstanceOf[ForeignKeyField].key == key) {
          Some(Concept(reg.fields.head.asInstanceOf[ForeignKeyField].key))
        }
        else None
    }
  }.toSet

  def isTypeOf = {
    key.storageManager.getMapFor(registers.ConceptTypeRelation).flatMap {
      case (_, reg) =>
        if (reg.fields.head.asInstanceOf[ForeignKeyField].key == key) {
          Some(Concept(reg.fields(1).asInstanceOf[ForeignKeyField].key))
        }
        else None
    }
  }.toSet
}

object Concept extends ElementFactory[registers.Concept, Concept] {
  def from(manager: LinkedStorageManager, hint: String): Option[Concept] = {
    from(manager, registers.Concept(hint))
  }
}
