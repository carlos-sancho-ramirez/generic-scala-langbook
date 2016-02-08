package sword.langbook.db

import sword.db.{CollectionReferenceField, ForeignKeyField, StorageManager}

case class Word(key :StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def languageKeyOpt = fields.collectFirst {
    case field :ForeignKeyField if field.definition.target == registers.Language => field.key
  }
  def pieceArrayIdOpt = fields.collectFirst {
    case field :CollectionReferenceField if field.definition.target == registers.PiecePosition => field.collectionId
  }

  def language = Language(languageKeyOpt.get)
  def pieces = PieceArray(key.storageManager, pieceArrayIdOpt.get)

  /**
    * Returns in a string the representation of this word in the given alphabet if any.
    * @return A Some instance containing the string or None if there is no representation for the
    *         given alphabet.
    */
  def text(alphabet: Alphabet): Option[String] = {
    // TODO: To be improved to return None instead of Some("") for wrong alphabets
    Some(pieces.flatMap(_.get(alphabet)).flatMap(x => x).map(_.unicode.toChar).mkString(""))
  }
}
