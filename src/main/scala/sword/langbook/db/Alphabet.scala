package sword.langbook.db

import sword.db.StorageManager

case class Alphabet(key :StorageManager.Key) {
  def registerOpt = key.storageManager.get(key).collectFirst { case reg :registers.Alphabet => reg }
  def concept = registerOpt.flatMap(reg => key.storageManager.get(reg.concept)).get

  def languages = key.storageManager.getMapFor(registers.LanguageAlphabet).values.filter(
    reg => LanguageAlphabet.alphabetKeyExtractor(reg) == key
  ).map(reg => Language(LanguageAlphabet.languageKeyExtractor(reg)))
}
