package sword.langbook.db

import sword.db.{Register, StorageManager}

case class SymbolArray(storageManager: StorageManager, textKey: StorageManager.Key) extends Seq[Symbol] {

  private def textReg = storageManager.get(textKey).get.asInstanceOf[redundant.Text]
  def arrayId: Register.CollectionId = {
    val id = textReg.symbolArray
    if (id == 0) {
      throw new AssertionError(s"Array does not exist for text with key '$textKey'")
    }
    id
  }

  private def symbols = storageManager.getArray(registers.SymbolPosition, arrayId).map(symbolPosition => Symbol(symbolPosition.symbol))

  override def length = textReg.text.length
  override def apply(idx :Int) = symbols(idx)
  override def iterator = symbols.iterator

  def alphabetsWhereIncluded = {
    val set = storageManager.getMapFor(registers.WordRepresentation, registers.WordRepresentation.SymbolArrayReferenceField(arrayId))
      .map(_._2.alphabet).toSet
    set.map(k => Alphabet(k))
  }

  def text: String = textReg.text
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
      (v.fields.head.value, k)
    }

    val paramRegisters = for (char <- text) yield {
      currentSymbols.get(char.toInt).map(key => registers.SymbolPosition(key)).get
    }

    for {
      arrayId <- storageManager.insert(paramRegisters)
      textKey <- storageManager.insert(redundant.Text(arrayId, text))
    } yield {
      apply(storageManager, textKey)
    }
  }
}
