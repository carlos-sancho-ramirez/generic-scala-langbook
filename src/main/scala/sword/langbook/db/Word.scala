package sword.langbook.db

import sword.db.{CollectionReferenceField, ForeignKeyField, StorageManager}

import scala.collection

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

  lazy val concepts = new scala.collection.mutable.Set[Concept]() {
    // TODO: This should check if the concept is already included, and avoid inserting anything in that case
    override def +=(elem: Concept): this.type = {
      val reg = registers.WordConcept(key, elem.key)
      key.storageManager.insert(reg)
      this
    }

    override def -=(elem: Concept): this.type = ???

    override def contains(elem: Concept): Boolean = {
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
      }.toSet.contains(elem)
    }

    override def iterator = new Iterator[Concept]() {
      val it = key.storageManager.getMapFor(registers.WordConcept).values.iterator

      def findNextConcept: Concept = {
        var result: Concept = null
        while(result == null && it.hasNext) {
          val reg = it.next()
          if (reg.fields.collectFirst {
            case field: ForeignKeyField if field.definition.target == registers.Word =>
              field.key.index
          }.contains(key.index)) {
            reg.fields.collectFirst {
              case field: ForeignKeyField if field.definition.target == registers.Concept =>
                Concept(field.key)
            }.foreach(concept => result = concept)
          }
        }

        result
      }

      var nextConcept = findNextConcept

      override def hasNext = nextConcept != null
      override def next() = {
        val next = nextConcept
        nextConcept = findNextConcept
        next
      }
    }
  }

  lazy val synonyms = new scala.collection.AbstractSet[Word]() {
    override def contains(elem: Word) = {
      elem.key != key && concepts.flatMap(_.wordsForLanguage(language)).contains(elem)
    }

    override def +(elem: Word): collection.Set[Word] = ???

    override def -(elem: Word): collection.Set[Word] = ???

    override def iterator = concepts.toSet[Concept].flatMap(_.wordsForLanguage(language)).filterNot(_.key == key).iterator
  }

  def translations: Set[Word] = {
    val thisLanguage = language
    concepts.toSet[Concept].flatMap(_.words).filterNot(_.language == thisLanguage)
  }
}

object Word extends ElementFactory[registers.Word, Word] {
  def from(manager: LinkedStorageManager, language: Language, pieces: PieceArray): Option[Word] = {
    from(manager, registers.Word(language.key, pieces.arrayId))
  }
}
