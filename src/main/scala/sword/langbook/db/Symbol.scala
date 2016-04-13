package sword.langbook.db

import sword.db.{Register, UnicodeField, StorageManager}

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
}

object Symbol extends ElementFactory[registers.Symbol, Symbol] {
  def from(manager: LinkedStorageManager, unicode: Register.UnicodeType): Option[Symbol] = {
    from(manager, registers.Symbol(unicode))
  }
}
