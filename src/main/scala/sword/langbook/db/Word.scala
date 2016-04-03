package sword.langbook.db

import sword.db.{CollectionReferenceField, ForeignKeyField, StorageManager}
import sword.langbook.db.registers.WordReferenceField

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

  /**
   * Return the suitable human readable string for this word based on the preferredAlphabet
    *
    * @return A Some instance with the string inside, or None if no text is found for this word.
   */
  def suitableText: Option[String] = {
    val preferredText = text.get(language.preferredAlphabet)
    if (preferredText.isEmpty) text.values.headOption else preferredText
  }

  lazy val concepts = new scala.collection.mutable.Set[Concept]() {

    private def filteredWordConceptMap = {
      key.storageManager.getMapFor(registers.WordConcept, WordReferenceField(key))
    }

    // TODO: This should check if the concept is already included, and avoid inserting anything in that case
    override def +=(elem: Concept): this.type = {
      val reg = registers.WordConcept(key, elem.key)
      key.storageManager.insert(reg)
      this
    }

    override def -=(elem: Concept): this.type = ???

    override def contains(elem: Concept): Boolean = {
      filteredWordConceptMap.flatMap {
        case (_,reg) =>
          reg.fields.collectFirst {
            case field: ForeignKeyField if field.definition.target == registers.Concept =>
              Concept(field.key)
          }
      }.toSet.contains(elem)
    }

    override def iterator = new Iterator[Concept]() {
      val it = filteredWordConceptMap.values.iterator

      def findNextConcept: Concept = {
        var result: Concept = null
        while(result == null && it.hasNext) {
          val reg = it.next()
          reg.fields.collectFirst {
            case field: ForeignKeyField if field.definition.target == registers.Concept =>
              Concept(field.key)
          }.foreach(concept => result = concept)
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
    private def wrappedSet = concepts.flatMap(_.wordsForLanguage(language))
    private def filteredWrappedSet = wrappedSet.filterNot(_.key == key)

    override def contains(elem: Word) = elem.key != key && wrappedSet.contains(elem)
    override def +(elem: Word): collection.Set[Word] = filteredWrappedSet + elem
    override def -(elem: Word): collection.Set[Word] = filteredWrappedSet - elem
    override def iterator = filteredWrappedSet.iterator
  }

  lazy val translations = new scala.collection.AbstractSet[Word]() {
    private def wrappedSet = concepts.flatMap(_.words).filterNot(_.language == language)

    override def contains(elem: Word) = wrappedSet.contains(elem)
    override def +(elem: Word): collection.Set[Word] = wrappedSet + elem
    override def -(elem: Word): collection.Set[Word] = wrappedSet + elem
    override def iterator = wrappedSet.iterator
  }
}

object Word extends ElementFactory[registers.Word, Word] {
  def from(manager: LinkedStorageManager, language: Language, pieces: PieceArray): Option[Word] = {
    from(manager, registers.Word(language.key, pieces.arrayId))
  }
}
