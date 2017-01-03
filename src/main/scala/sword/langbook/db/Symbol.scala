package sword.langbook.db

import sword.db.StorageManager.Key
import sword.db.Register

case class Symbol(key: Key) {
  if (key.registerDefinition != registers.Symbol) {
    throw new IllegalArgumentException("Wrong registerDefinition within the key for a Symbol")
  }

  def unicode = key.registerOption.get.asInstanceOf[registers.Symbol].unicode
  def text = "" + unicode.toChar

  def alphabetsWhereIncluded = key.storageManager.alphabetsWhereSymbolIncluded(key).map(Alphabet(_))
}

object Symbol extends ElementFactory[registers.Symbol, Symbol] {
  def from(manager: LinkedStorageManager, unicode: Register.UnicodeType): Option[Symbol] = {
    from(manager, registers.Symbol(unicode))
  }
}
