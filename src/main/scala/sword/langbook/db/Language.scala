package sword.langbook.db

import sword.db.{Register, StorageManager}
import sword.langbook.db.registers.Concept

case class Language(storageManager :StorageManager, key :StorageManager.Key) {
  def registerOpt = storageManager.get(key).collectFirst { case reg :registers.Language => reg }

  def concept = registerOpt.flatMap(reg => storageManager.get(reg.concept)).get

  def alphabets = storageManager.getMapFor(registers.LanguageAlphabet).values.filter(
    reg => LanguageAlphabet.languageKeyExtractor(reg) == key
  ).map(reg => Alphabet(storageManager, LanguageAlphabet.alphabetKeyExtractor(reg)))
}
