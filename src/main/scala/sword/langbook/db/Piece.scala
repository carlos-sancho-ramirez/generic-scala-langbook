package sword.langbook.db

import sword.db.{Register, StorageManager}

case class Piece(storageManager :StorageManager, collectionId :Register.CollectionId) extends
    scala.collection.mutable.Map[Alphabet, SymbolArray] {

  private def wrappedCollection = storageManager.getCollection(registers.Piece, collectionId)
  private def wrappedMap = wrappedCollection.map(piece => (Alphabet(piece.alphabet), SymbolArray(storageManager, piece.symbolArray))).toMap

  override def get(key: Alphabet) = wrappedMap.get(key)
  override def iterator = wrappedMap.iterator
  override def +=(kv: (Alphabet, SymbolArray)): Piece.this.type = {
    val reg = registers.Piece(kv._1.key, kv._2.arrayId)
    storageManager.insert(collectionId, reg)
    this
  }

  override def -=(key: Alphabet): Piece.this.type = ???

  override def keysIterator = wrappedCollection.map(piece => Alphabet(piece.alphabet)).iterator
  override def size = {
    storageManager.getKeysForCollection(registers.Piece, collectionId).size
  }

  object text extends scala.collection.Map[Alphabet,String] {
    override def get(key: Alphabet): Option[String] = {
      storageManager.getCollection(registers.Piece, collectionId).find(_.alphabet == key.key)
          .map(piece => SymbolArray(storageManager, piece.symbolArray).text)
    }

    private def wrapped = wrappedMap.mapValues(_.text)
    override def iterator: Iterator[(Alphabet, String)] = wrapped.iterator
    override def +[B1 >: String](kv: (Alphabet, B1)): collection.Map[Alphabet, B1] = wrapped + kv
    override def -(key: Alphabet): collection.Map[Alphabet, String] = wrapped - key

    override def keysIterator = Piece.this.keysIterator
    override def size = Piece.this.size
  }
}

object Piece extends {
  def from(manager: LinkedStorageManager, map: scala.collection.Map[Alphabet, SymbolArray]): Option[Piece] = {
    val regs = for {
      (alphabet, symbolArray) <- map
    } yield {
      registers.Piece(alphabet.key, symbolArray.arrayId)
    }

    manager.storageManager.insert(regs).map(apply(manager.storageManager, _))
  }

  def from(manager: LinkedStorageManager, alphabet: Alphabet, symbolArray: SymbolArray): Option[Piece] = {
    from(manager, Map(alphabet -> symbolArray))
  }
}
