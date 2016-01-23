package sword.langbook.db

import sword.db.{Register, StorageManager}

case class SymbolArray(storageManager :StorageManager, arrayId :Register.CollectionId) extends Seq[Symbol] {

  def symbolKeys = storageManager.getKeysForArray(registers.SymbolPosition, arrayId)
  override def length = symbolKeys.length
  override def apply(idx :Int) = Symbol(symbolKeys(idx))

  override def iterator = new Iterator[Symbol] {
    val symbols = symbolKeys.iterator
    override def hasNext = symbols.hasNext
    override def next() = Symbol(symbols.next())
  }
}
