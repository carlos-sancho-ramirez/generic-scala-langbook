package sword.langbook.db

import sword.db.{StorageManager, Register}
import sword.langbook.db.registers.Concept

case class Alphabet(storageManager :StorageManager, key :Register.Key) {
  def registerOpt = storageManager.get(registers.Alphabet, key).collectFirst { case reg :registers.Alphabet => reg }
  def concept = registerOpt.flatMap(reg => storageManager.get(Concept, reg.concept)).get

  def languages = storageManager.getMapFor(registers.LanguageAlphabet).values.filter(
    reg => LanguageAlphabet.alphabetKeyExtractor(reg) == key
  ).map(reg => Language(storageManager, LanguageAlphabet.languageKeyExtractor(reg)))
}
