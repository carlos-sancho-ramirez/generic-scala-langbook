package sword.langbook.db

import sword.db.{CollectionReferenceField, ForeignKeyField, StorageManager}
import sword.langbook.db.registers.WordReferenceField

import scala.collection

case class Word(key :StorageManager.Key) {
  private def wordReg = key.registerOption.get.asInstanceOf[registers.Word]
  def language = Language(wordReg.language)
  def representation = Representation(key.storageManager, key)
  def text = representation.text

  /**
   * Return the suitable human readable string for this word based on the preferredAlphabet
    *
    * @return A Some instance with the string inside, or None if no text is found for this word.
   */
  def suitableText: Option[String] = {
    val preferredText = text.get(language.preferredAlphabet)
    if (preferredText.isEmpty) text.values.headOption else preferredText
  }

  object concepts extends scala.collection.mutable.Set[Concept]() {

    private def filteredWordConcepts = {
      key.storageManager.getMapFor(registers.WordConcept, WordReferenceField(key)).values
    }

    // TODO: This should check if the concept is already included, and avoid inserting anything in that case
    override def +=(elem: Concept): this.type = {
      val reg = registers.WordConcept(key, elem.key)
      key.storageManager.insert(reg)
      this
    }

    override def -=(elem: Concept): this.type = ???

    override def contains(elem: Concept): Boolean = {
      filteredWordConcepts.map(reg => Concept(reg.concept)).toSet.contains(elem)
    }

    override def iterator = new Iterator[Concept]() {
      val it = filteredWordConcepts.iterator

      def findNextConcept: Concept = {
        if (it.hasNext) Concept(it.next().concept)
        else null
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
  def from(manager: LinkedStorageManager, language: Language): Option[Word] = {
    from(manager, registers.Word(language.key))
  }
}
