package sword.langbook.db

import sword.db.{Register, CollectionReferenceField, ForeignKeyField, StorageManager}

case class Piece(storageManager :StorageManager, collectionId :Register.CollectionId) extends
    Map[Alphabet, SymbolArray] {
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

  override def +[B1 >: SymbolArray](kv: (Alphabet, B1)): Map[Alphabet, B1] = ???
  override def -(key: Alphabet): Map[Alphabet, SymbolArray] = ???
}
