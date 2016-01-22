package sword.langbook.db

import sword.db.{CharSequenceField, StorageManager}

case class Concept(key :StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def hintOpt = fields.collectFirst {
    case field :CharSequenceField => field.value
  }

  def hint = hintOpt.get

  def languages = key.storageManager.getMapFor(registers.LanguageAlphabet).values.filter(
    reg => LanguageAlphabet.alphabetKeyExtractor(reg) == key
  ).map(reg => Language(LanguageAlphabet.languageKeyExtractor(reg)))
}
