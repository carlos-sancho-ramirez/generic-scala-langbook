package sword.langbook.db

import sword.db.{Register, CollectionReferenceField, ForeignKeyField, StorageManager}

case class Piece(storageManager :StorageManager, collectionId :Register.CollectionId) extends
    scala.collection.mutable.Map[Alphabet, SymbolArray] {
  def regKeys = storageManager.getKeysForCollection(registers.Piece, collectionId)
  def fields(key :StorageManager.Key) = key.registerOption.map(_.fields).getOrElse(Seq())
  def alphabetKeyOpt(key :StorageManager.Key) = fields(key).collectFirst {
    case field :ForeignKeyField if field.definition.target == registers.Alphabet => field.key
  }
  def symbolArrayIdOpt(key :StorageManager.Key) = fields(key).collectFirst {
    case field :CollectionReferenceField if field.definition.target == registers.SymbolPosition => field.collectionId
  }

  override def get(key: Alphabet): Option[SymbolArray] = {
    val pieceKey = regKeys.flatMap(k => alphabetKeyOpt(k).map(alphabetKey => (k,alphabetKey)))
      .filter { case (_,alphabetKey) => alphabetKey.index == key.key.index }.map(_._1).headOption

    pieceKey.flatMap(k => symbolArrayIdOpt(k).map(SymbolArray(storageManager, _)))
  }

  override def iterator: Iterator[(Alphabet, SymbolArray)] = new Iterator[(Alphabet, SymbolArray)] {
    val it = regKeys.iterator
    override def hasNext = it.hasNext
    override def next(): (Alphabet, SymbolArray) = {
      val key = it.next()
      (Alphabet(alphabetKeyOpt(key).get), SymbolArray(storageManager, symbolArrayIdOpt(key).get))
    }
  }

  override def +=(kv: (Alphabet, SymbolArray)): Piece.this.type = {
    val reg = registers.Piece(kv._1.key, kv._2.arrayId)
    storageManager.insert(collectionId, reg)
    this
  }

  override def -=(key: Alphabet): Piece.this.type = ???
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