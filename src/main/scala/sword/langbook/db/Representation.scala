package sword.langbook.db

import sword.db.StorageManager

case class Representation(storageManager :StorageManager, wordKey :StorageManager.Key)
    extends scala.collection.mutable.Map[Alphabet, SymbolArray] {

  private val redundantWordKey = storageManager.getMapFor(redundant.RedundantWord, redundant.RedundantWord.WordReferenceField(wordKey)).head._1
  private def wordReference = registers.WordRepresentation.WordReferenceField(wordKey)
  private def wordTexts = storageManager
    .getMapFor(redundant.WordText, redundant.WordText.RedundantWordReferenceField(redundantWordKey)).values
  private def wrappedMap = wordTexts.map(repr => (Alphabet(repr.alphabet),
      SymbolArray(storageManager, repr.text))).toMap

  override def get(key: Alphabet) = wrappedMap.get(key)
  override def iterator = wrappedMap.iterator
  override def +=(kv: (Alphabet, SymbolArray)): Representation.this.type = {
    val reg = redundant.WordText(wordKey, kv._1.key, kv._2.textKey)
    storageManager.insert(reg)
    this
  }

  override def -=(key: Alphabet): Representation.this.type = ???

  override def keysIterator = wordTexts.map(r => Alphabet(r.alphabet)).iterator
  override def size = {
    storageManager.getKeysFor(registers.WordRepresentation, wordReference).size
  }

  object text extends scala.collection.Map[Alphabet, String] {
    override def get(key: Alphabet): Option[String] = wordTexts
        .find(_.alphabet == key.key).map(repr => SymbolArray(storageManager, repr.text).text)

    private def wrapped = wrappedMap.mapValues(_.text)
    override def iterator: Iterator[(Alphabet, String)] = wrapped.iterator
    override def +[B1 >: String](kv: (Alphabet, B1)): collection.Map[Alphabet, B1] = wrapped + kv
    override def -(key: Alphabet): collection.Map[Alphabet, String] = wrapped - key

    override def keysIterator = Representation.this.keysIterator
    override def size = Representation.this.size
  }
}
