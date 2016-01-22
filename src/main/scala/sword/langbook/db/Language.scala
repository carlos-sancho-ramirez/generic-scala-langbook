package sword.langbook.db

import sword.db.{ForeignKeyField, StorageManager}

case class Language(key :StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def conceptKeyOpt = fields.collectFirst {
    case field :ForeignKeyField if field.definition.target == registers.Concept => field.key
  }

  def concept = Concept(conceptKeyOpt.get)

  def alphabets = key.storageManager.getMapFor(registers.LanguageAlphabet).values.filter(
    reg => LanguageAlphabet.languageKeyExtractor(reg) == key
  ).map(reg => Alphabet(LanguageAlphabet.alphabetKeyExtractor(reg)))
}
