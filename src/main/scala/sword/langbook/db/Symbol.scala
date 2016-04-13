package sword.langbook.db

import sword.db.{Register, UnicodeField, StorageManager}
import sword.langbook.db.registers.SymbolReferenceField

case class Symbol(key :StorageManager.Key) {
  if (key.registerDefinition != registers.Symbol) {
    throw new IllegalArgumentException("Wrong registerDefinition within the key for a Symbol")
  }

  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def unicodeOpt = fields.collectFirst {
    case field :UnicodeField => field.value
  }

  def unicode = unicodeOpt.get
  def text = unicodeOpt.map("" + _.toChar).get

  def arraysWhereIncluded = {
    val storageManager = key.storageManager
    storageManager.getMapFor(registers.SymbolPosition, SymbolReferenceField(key)).keys.map(k => SymbolArray(storageManager, k.group)).toSet
  }

  def alphabetsWhereIncluded = arraysWhereIncluded.flatMap(_.alphabetsWhereIncluded)
}

object Symbol extends ElementFactory[registers.Symbol, Symbol] {
  def from(manager: LinkedStorageManager, unicode: Register.UnicodeType): Option[Symbol] = {
    from(manager, registers.Symbol(unicode))
  }
}
