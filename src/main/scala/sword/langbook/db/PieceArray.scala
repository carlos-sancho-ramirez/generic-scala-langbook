package sword.langbook.db

import sword.db.{CollectionReferenceField, Register, StorageManager}

import scala.collection.AbstractIterator

case class PieceArray(storageManager :StorageManager, arrayId :Register.CollectionId) extends Seq[Piece] {

  def pieceKeys = storageManager.getKeysForArray(registers.PiecePosition, arrayId)
  def fields(key :StorageManager.Key) = key.registerOption.map(_.fields).getOrElse(Seq())
  def pieceCollIdOpt(key :StorageManager.Key) = fields(key).collectFirst {
    case field :CollectionReferenceField if field.definition.target == registers.Piece => field.collectionId
  }

  override def length = pieceKeys.length
  override def apply(idx :Int) = Piece(storageManager, pieceCollIdOpt(pieceKeys(idx)).get)

  override def iterator = new Iterator[Piece] {
    val keys = pieceKeys.iterator
    override def hasNext = keys.hasNext
    override def next() = Piece(storageManager, pieceCollIdOpt(keys.next()).get)
  }

  /**
    * Returns a map match alphabets and its string representations
    */
  def text: Map[Alphabet, String] = new Map[Alphabet, String] {

    /**
     * So far, valid alphabet are those that all pieces includes
     */
    private def validAlphabets = PieceArray.this.map(_.text.keySet).reduce((a,b) => a.intersect(b))

    private def trustyGet(alphabet :Alphabet) = {
      PieceArray.this.map(_.text(alphabet)).mkString
    }

    override def get(alphabet: Alphabet): Option[String] = {
      PieceArray.this.foldLeft(Option(""))((strOpt, piece) =>
          strOpt.flatMap(str => piece.text.get(alphabet).map(str + _)))
    }

    override def iterator = new AbstractIterator[(Alphabet, String)] {
      private val it = validAlphabets.iterator

      override def hasNext = it.hasNext
      override def next() = {
        val alphabet = it.next()

        // Here we are assuming that the value will be there while still iterating.
        // trustyGet could crash if something changed in the meanwhile
        // TODO: Avoid any crash
        (alphabet, trustyGet(alphabet))
      }
    }

    // TODO: Implement these methods
    override def +[B1 >: String](kv: (Alphabet, B1)): Map[Alphabet, B1] = ???
    override def -(key: Alphabet): Map[Alphabet, String] = ???
  }
}

object PieceArray {
  def from(manager: LinkedStorageManager, pieces: Seq[Piece]): Option[PieceArray] = {
    val storageManager = manager.storageManager
    val positions = pieces.map(piece => registers.PiecePosition(piece.collectionId))
    storageManager.insert(positions).map(apply(storageManager,_))
  }
}