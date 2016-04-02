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

    private def wrappedSetForPreferred = key.storageManager.getMapFor(registers.Language).flatMap {
      case (languageKey, language) =>
        language.fields.collectFirst {
          case f: ForeignKeyField if f.definition.target == registers.Alphabet && f.key == key => Language(languageKey)
        }
    }.toSet

    // TODO: Looking in pieces for alphabets including this as preferred alphabet should be avoided for performance reasons
    private def wrappedSetForIncludedInPieces = {
      val storageManager = key.storageManager
      storageManager.getMapFor(registers.Language).filter {
        case (languageKey, language) =>
          storageManager.getMapFor(registers.Word).exists {
            case (wordKey, wordReg) =>
              wordReg.fields.collectFirst {
                case f: ForeignKeyField if f.definition.target == registers.Language && f.key == languageKey => f
              }.isDefined && {
                val pieceArrayCollIdOpt = wordReg.fields.collectFirst {
                  case f: CollectionReferenceField if f.definition.target == registers.PiecePosition => f.collectionId
                }
                pieceArrayCollIdOpt.exists { pieceArrayCollId =>
                  val pieces = storageManager.getMapForCollection(registers.PiecePosition, pieceArrayCollId).values.flatMap {
                    _.fields.collectFirst {
                      case f: CollectionReferenceField if f.definition.target == registers.Piece => f.collectionId
                    }
                  }
                  pieces.exists { pieceId =>
                    storageManager.getMapForCollection(registers.Piece, pieceId).values.exists {
                      case pieceReg =>
                        pieceReg.fields.collectFirst {
                          case f: ForeignKeyField if f.definition.target == registers.Alphabet && f.key == key => key
                        }.isDefined
                    }
                  }
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
}

object Alphabet extends ElementFactory[registers.Alphabet, Alphabet] {
  def from(manager: LinkedStorageManager, concept: Concept): Option[Alphabet] = {
    from(manager, registers.Alphabet(concept.key))
  }
}
