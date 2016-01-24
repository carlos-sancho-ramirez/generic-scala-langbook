package sword.langbook.db

import sword.db.{ForeignKeyField, Register, StorageManager}

case class SymbolArray(storageManager :StorageManager, arrayId :Register.CollectionId) extends Seq[Symbol] {

  def symbolKeys = storageManager.getKeysForArray(registers.SymbolPosition, arrayId)
  if (symbolKeys.isEmpty) {
    throw new IllegalArgumentException("Wrong arrayId for a SymbolArray")
  }

  def targetSymbolKey(key :StorageManager.Key) = key.registerOption.flatMap(_.fields.collectFirst {
    case field :ForeignKeyField if field.definition.target == registers.Symbol => field.key
  })

  override def length = symbolKeys.length
  override def apply(idx :Int) = Symbol(symbolKeys(idx))

  override def iterator = new Iterator[Symbol] {
    val symbols = symbolKeys.iterator
    override def hasNext = symbols.hasNext
    override def next() = Symbol(targetSymbolKey(symbols.next()).get)
  }
}
