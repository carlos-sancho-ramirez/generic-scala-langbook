package sword.langbook.db

import sword.db.{UnicodeField, ForeignKeyField, Register, StorageManager}
import sword.langbook.db.registers.SymbolArrayReferenceField

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

  def alphabetsWhereIncluded = {
    storageManager.getMapFor(registers.Piece, SymbolArrayReferenceField(arrayId)).values
        .map(piece => Alphabet(piece.alphabet)).toSet
  }
}

object SymbolArray {

  def from(manager: LinkedStorageManager, text: String): Option[SymbolArray] = {
    val storageManager = manager.storageManager
    val originalSymbols = storageManager.getMapFor(registers.Symbol).values.map(_.fields.head
      .asInstanceOf[sword.db.UnicodeField].value).toSet
    val symbolsToAdd = text.toSet[Char].map(_.toInt).diff(originalSymbols)

    for (symbol <- symbolsToAdd) {
      // Currently if there is an error inserting any of the symbols we will be unable to register
      // the word properly, so none of them should be added in that case to avoid orfan symbols
      // TODO: Find a strategy to include all only if the word can be added or nothing at all if not
      storageManager.insert(registers.Symbol(symbol))
    }

    val currentSymbols = storageManager.getMapFor(registers.Symbol).map { case (k, v) =>
      (v.fields.head.asInstanceOf[sword.db.UnicodeField].value, k)
    }

    val paramRegisters = for (char <- text) yield {
      currentSymbols.get(char.toInt).map(key => registers.SymbolPosition(key)).get
    }

    storageManager.insert(paramRegisters).map(apply(storageManager, _))
  }
}
