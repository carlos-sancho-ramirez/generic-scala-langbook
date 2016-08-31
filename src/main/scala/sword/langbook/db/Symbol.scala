package sword.langbook.db

import sword.db.{Register, UnicodeField, StorageManager}
import sword.langbook.db.registers.SymbolReferenceField

case class Symbol(key :StorageManager.Key) {
  if (key.registerDefinition != registers.Symbol) {
    throw new IllegalArgumentException("Wrong registerDefinition within the key for a Symbol")
  }

  def unicode = key.registerOption.get.asInstanceOf[registers.Symbol].unicode
  def text = "" + unicode.toChar

  def arraysWhereIncluded = {
    val storageManager = key.storageManager
    storageManager.getKeysFor(registers.SymbolPosition, SymbolReferenceField(key))
        .map(k => SymbolArray(storageManager, k.group))
  }

  //def alphabetsWhereIncluded = arraysWhereIncluded.flatMap(_.alphabetsWhereIncluded)
  def alphabetsWhereIncluded = key.storageManager.alphabetsWhereSymbolIncluded(key).map(Alphabet(_))
}

object Symbol extends ElementFactory[registers.Symbol, Symbol] {
  def from(manager: LinkedStorageManager, unicode: Register.UnicodeType): Option[Symbol] = {
    from(manager, registers.Symbol(unicode))
  }
}
