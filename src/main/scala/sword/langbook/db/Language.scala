package sword.langbook.db

import sword.db.{Register, StorageManager}
import sword.langbook.db.registers.Concept

case class Language(storageManager :StorageManager, key :Register.Key) {
  def registerOpt = storageManager.get(registers.Language, key).collectFirst { case reg :registers.Language => reg }

  def concept = registerOpt.flatMap(reg => storageManager.get(Concept, reg.concept)).get

  def alphabets = storageManager.getMapFor(registers.LanguageAlphabet).values.filter(
    reg => LanguageAlphabet.languageKeyExtractor(reg) == key
  ).map(reg => Alphabet(storageManager, LanguageAlphabet.alphabetKeyExtractor(reg)))
}
