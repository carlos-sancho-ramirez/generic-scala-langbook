package sword.langbook.db

import sword.db.{Register, StorageManager}
import sword.langbook.db.registers.Concept

case class Language(key :StorageManager.Key) {
  def registerOpt = key.storageManager.get(key).collectFirst { case reg :registers.Language => reg }

  def concept = registerOpt.flatMap(reg => key.storageManager.get(reg.concept)).get

  def alphabets = key.storageManager.getMapFor(registers.LanguageAlphabet).values.filter(
    reg => LanguageAlphabet.languageKeyExtractor(reg) == key
  ).map(reg => Alphabet(LanguageAlphabet.alphabetKeyExtractor(reg)))
}
