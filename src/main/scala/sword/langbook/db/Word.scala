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
    * Returns a map match alphabets and its string representations
    */
  def text: Map[Alphabet, String] = new Map[Alphabet, String] {
    private def trustyGet(alphabet :Alphabet) = {
      pieces.flatMap(_.get(alphabet)).flatMap(x => x).map(_.unicode.toChar).mkString("")
    }

    override def get(alphabet: Alphabet): Option[String] = {
      if (pieces.forall(_.contains(alphabet))) {
        Some(trustyGet(alphabet))
      }
      else None
    }

    override def iterator: Iterator[(Alphabet, String)] = new Iterator[(Alphabet, String)] {
      private val it = pieces.foldLeft(Set[Alphabet]())((set, piece) => set ++ piece.keys).filter {
        alphabet => pieces.forall(_.contains(alphabet))
      }.iterator

      override def hasNext = it.hasNext
      override def next(): (Alphabet, String) = {
        val alphabet = it.next()
        (alphabet, trustyGet(alphabet))
      }
    }

    // TODO: Implement these methods
    override def +[B1 >: String](kv: (Alphabet, B1)): Map[Alphabet, B1] = ???
    override def -(key: Alphabet): Map[Alphabet, String] = ???
  }

  /**
    * Return all concepts attached to this word.
    */
  def concepts: Set[Concept] = {
    key.storageManager.getMapFor(registers.WordConcept).values.filter {
      reg =>
        reg.fields.collectFirst {
          case field: ForeignKeyField if field.definition.target == registers.Word =>
            field.key.index
        }.contains(key.index)
    }.flatMap {
      reg =>
        reg.fields.collectFirst {
          case field: ForeignKeyField if field.definition.target == registers.Concept =>
            Concept(field.key)
        }
    }.toSet
  }

  def synonyms: Set[Word] = concepts.flatMap(_.wordsForLanguage(language)).filterNot(_.key == key)

  def translations: Set[Word] = {
    val thisLanguage = language
    concepts.flatMap(_.words).filterNot(_.language == thisLanguage)
  }
}

object Word extends ElementFactory[registers.Word, Word] {
  def from(manager: LinkedStorageManager, language: Language, pieces: PieceArray): Option[Word] = {
    from(manager, registers.Word(language.key, pieces.arrayId))
  }
}