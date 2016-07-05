package sword.langbook.db

import sword.db.StorageManager
import sword.langbook.db.registers.WordReferenceField

case class Representation(storageManager :StorageManager, wordKey :StorageManager.Key)
    extends scala.collection.mutable.Map[Alphabet, SymbolArray] {

  private def wordReference = WordReferenceField(wordKey)
  private def wordRepresentations = storageManager
      .getMapFor(registers.WordRepresentation, wordReference).values
  private def wrappedMap = wordRepresentations.map(repr => (Alphabet(repr.alphabet),
      SymbolArray(storageManager, repr.symbolArray))).toMap

  override def get(key: Alphabet) = wrappedMap.get(key)
  override def iterator = wrappedMap.iterator
  override def +=(kv: (Alphabet, SymbolArray)): Representation.this.type = {
    val reg = registers.WordRepresentation(wordKey, kv._1.key, kv._2.arrayId)
    storageManager.insert(reg)
    this
  }

  override def -=(key: Alphabet): Representation.this.type = ???

  override def keysIterator = wordRepresentations.map(r => Alphabet(r.alphabet)).iterator
  override def size = {
    storageManager.getKeysFor(registers.WordRepresentation, wordReference).size
  }

  object text extends scala.collection.Map[Alphabet,String] {
    override def get(key: Alphabet): Option[String] = wordRepresentations
        .find(_.alphabet == key.key).map(repr => SymbolArray(storageManager, repr.symbolArray).text)

    private def wrapped = wrappedMap.mapValues(_.text)
    override def iterator: Iterator[(Alphabet, String)] = wrapped.iterator
    override def +[B1 >: String](kv: (Alphabet, B1)): collection.Map[Alphabet, B1] = wrapped + kv
    override def -(key: Alphabet): collection.Map[Alphabet, String] = wrapped - key

    override def keysIterator = Representation.this.keysIterator
    override def size = Representation.this.size
  }
}
