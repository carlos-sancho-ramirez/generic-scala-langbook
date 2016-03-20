package sword.langbook.db

import sword.db.{CollectionReferenceField, Register, StorageManager}

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
}

object PieceArray {
  def from(manager: LinkedStorageManager, pieces: Seq[Piece]): Option[PieceArray] = {
    val storageManager = manager.storageManager
    val positions = pieces.map(piece => registers.PiecePosition(piece.collectionId))
    storageManager.insert(positions).map(apply(storageManager,_))
  }
}