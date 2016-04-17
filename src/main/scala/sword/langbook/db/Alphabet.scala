package sword.langbook.db

import sword.db.{CollectionReferenceField, ForeignKeyField, StorageManager}
import sword.langbook.db.registers.{AlphabetReferenceField, PieceReferenceField}

case class Alphabet(key :StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def conceptKeyOpt = fields.collectFirst {
    case field :ForeignKeyField if field.definition.target == registers.Concept => field.key
  }

  def concept = Concept(conceptKeyOpt.get)

  lazy val languages = new scala.collection.AbstractSet[Language] {

    private def wrappedSetForPreferred = key.storageManager.getMapFor(registers.Language)
        .filter(_._2.preferredAlphabet == key).map(t => Language(t._1)).toSet

    // TODO: Looking in pieces for alphabets including this as preferred alphabet should be avoided for performance reasons
    private def wrappedSetForIncludedInPieces = {
      val storageManager = key.storageManager
      storageManager.getMapFor(registers.Language).filter {
        case (languageKey, language) =>
          storageManager.getMapFor(registers.Word).exists { case (_, wordReg) =>
            wordReg.language == languageKey && {
              storageManager.getCollection(registers.PiecePosition, wordReg.pieceArray).map(_.piece).exists { pieceId =>
                storageManager.getCollection(registers.Piece, pieceId).exists(_.alphabet == key)
              }
            }
          }
      }.map { case (languageKey,_) => Language(languageKey)}.toSet
    }

    private def wrappedSet = wrappedSetForPreferred ++ wrappedSetForIncludedInPieces

    override def contains(elem: Language) = wrappedSet.contains(elem)
    override def +(elem: Language) = wrappedSet + elem
    override def -(elem: Language) = wrappedSet - elem
    override def iterator = wrappedSet.iterator
  }

  /**
   * Returns a suitable human readable string for the given language if any
   * @param preferredLanguage Desired language that the user would like to read.
   */
  def suitableTextForLanguage(preferredLanguage: Language): Option[String] = {
    val preferredWordOption = concept.wordsForLanguage(preferredLanguage).headOption
    val wordOption = {
      if (preferredWordOption.isDefined) preferredWordOption
      else concept.words.headOption
    }

    wordOption.flatMap(_.suitableText)
  }

  def symbols = {
    val storageManager = key.storageManager
    val pieces = storageManager.getMapFor(registers.Piece, AlphabetReferenceField(key)).values
    val symbolArrays = pieces.map(reg => SymbolArray(storageManager, reg.symbolArray))
    symbolArrays.flatMap(x => x).toSet
  }
}

object Alphabet extends ElementFactory[registers.Alphabet, Alphabet] {
  def from(manager: LinkedStorageManager, concept: Concept): Option[Alphabet] = {
    from(manager, registers.Alphabet(concept.key))
  }
}
