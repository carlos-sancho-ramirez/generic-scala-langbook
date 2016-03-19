package sword.langbook.db

import sword.db.{ForeignKeyField, StorageManager}

case class Alphabet(key :StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def conceptKeyOpt = fields.collectFirst {
    case field :ForeignKeyField if field.definition.target == registers.Concept => field.key
  }

  def concept = Concept(conceptKeyOpt.get)

  // TODO: To be implemented
  def languages = ???
}

object Alphabet {
  def from(manager: LinkedStorageManager, register: registers.Alphabet): Option[Alphabet] = {
    manager.storageManager.insert(register).map(apply)
  }

  def from(manager: LinkedStorageManager, concept: Concept): Option[Alphabet] = {
    from(manager, registers.Alphabet(concept.key))
  }
}
