package sword.langbook.db

import sword.db.{ForeignKeyField, CharSequenceField, StorageManager}

case class Concept(key :StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def hintOpt = fields.collectFirst {
    case field :CharSequenceField => field.value
  }

  def hint = hintOpt.get

  def languages = key.storageManager.getMapFor(registers.LanguageAlphabet).values.filter(
    reg => LanguageAlphabet.alphabetKeyExtractor(reg) == key
  ).map(reg => Language(LanguageAlphabet.languageKeyExtractor(reg)))

  def words = {
    key.storageManager.getMapFor(registers.WordConcept).values.filter( reg =>
      reg.fields.collectFirst {
        case field: ForeignKeyField if field.definition.target == registers.Concept =>
          field.key.index
      }.contains(key.index)
    ).flatMap( reg =>
      reg.fields.collectFirst {
        case field: ForeignKeyField if field.definition.target == registers.Word =>
          Word(field.key)
      }
    )
  }

  def wordsForLanguage(language :Language) = {
    words.filter(_.language == language)
  }
}
