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
}

object Concept extends ElementFactory[registers.Concept, Concept] {
  def from(manager: LinkedStorageManager, hint: String): Option[Concept] = {
    from(manager, registers.Concept(hint))
  }
}