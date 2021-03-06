package sword.langbook.db

import sword.db.StorageManager

case class Word(override val key :StorageManager.Key) extends Selectable {
  private def wordReg = key.registerOption.get.asInstanceOf[registers.Word]
  private def redundantWordKey = key.storageManager.getMapFor(redundant.RedundantWord, redundant.RedundantWord.WordReferenceField(key)).keys.head

  def language = Language(wordReg.language)
  def representation = Representation(key)
  def text = representation.text

  override def suitableText: Option[String] = {
    val preferredText = text.get(language.preferredAlphabet)
    if (preferredText.isEmpty) text.values.headOption else preferredText
  }

  object concepts extends scala.collection.mutable.Set[Concept]() {

    private def filteredWordConcepts = {
      key.storageManager.getMapFor(registers.Acceptation, registers.Acceptation.WordReferenceField(key)).values
    }

    // TODO: This should check if the concept is already included, and avoid inserting anything in that case
    override def +=(elem: Concept): this.type = {
      val reg = registers.Acceptation(key, elem.key)
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

  lazy val bunches = key.storageManager.getMapFor(redundant.ResolvedBunch,
      redundant.ResolvedBunch.RedundantWordReferenceField(redundantWordKey)).map(pair => Bunch(pair._2.bunch))

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

  // TODO: Keys of this map should not be plain strings as they are not translatable
  lazy val morphologies: Map[String /* Bunch name */, String /* Word text */] = {
    val manager = key.storageManager
    val preferredAlphabet = language.preferredAlphabet.key
    val targetBunches = manager.getMapFor(registers.Agent).collect {
      case (_, agent) if registers.Agent.Flags.shouldModify(agent.flags) => agent.targetBunch
    }.toSet

    val redundantWordKeysForOriginal = manager.getMapFor(redundant.RedundantWord, redundant.RedundantWord.OriginalWordReferenceField(key)).keySet

    manager.getMapFor(redundant.ResolvedBunch).collect {
      case (_, reg) if targetBunches(reg.bunch) && redundantWordKeysForOriginal(reg.word) =>
        val wordTexts = manager.getMapFor(redundant.WordText, redundant.WordText.RedundantWordReferenceField(reg.word))
        val suitableWordText = wordTexts.find(_._2.alphabet == preferredAlphabet).getOrElse(wordTexts.head)
        val textKey = suitableWordText._2.text
        (Bunch(reg.bunch).name, manager.get(textKey).get.asInstanceOf[redundant.Text].text)
    }.toMap
  }
}

object Word extends ElementFactory[registers.Word, Word] {
  def from(manager: LinkedStorageManager, language: Language): Option[Word] = {
    from(manager, registers.Word(language.key))
  }

  val hiraganaConversions = List(
    "あ" -> "a",
    "い" -> "i",
    "う" -> "u",
    "え" -> "e",
    "お" -> "o",
    "きゃ" -> "kya",
    "きゅ" -> "kyu",
    "きょ" -> "kyo",
    "ぎゃ" -> "gya",
    "ぎゅ" -> "gyu",
    "ぎょ" -> "gyo",
    "か" -> "ka",
    "っか" -> "kka",
    "き" -> "ki",
    "っき" -> "kki",
    "く" -> "ku",
    "っく" -> "kku",
    "け" -> "ke",
    "っけ" -> "kke",
    "こ" -> "ko",
    "っこ" -> "kko",
    "が" -> "ga",
    "ぎ" -> "gi",
    "ぐ" -> "gu",
    "げ" -> "ge",
    "ご" -> "go",
    "しゃ" -> "sha",
    "しゅ" -> "shu",
    "しょ" -> "sho",
    "じゃ" -> "ja",
    "じゅ" -> "ju",
    "じょ" -> "jo",
    "さ" -> "sa",
    "し" -> "shi",
    "す" -> "su",
    "せ" -> "se",
    "そ" -> "so",
    "ざ" -> "za",
    "じ" -> "ji",
    "ず" -> "zu",
    "ぜ" -> "ze",
    "ぞ" -> "zo",
    "ちゃ" -> "cha",
    "ちゅ" -> "chu",
    "ちょ" -> "cho",
    "た" -> "ta",
    "った" -> "tta",
    "ち" -> "chi",
    "つ" -> "tsu",
    "て" -> "te",
    "って" -> "tte",
    "と" -> "to",
    "っと" -> "tto",
    "だ" -> "da",
    "ぢ" -> "di",
    "づ" -> "du",
    "で" -> "de",
    "ど" -> "do",
    "にゃ" -> "nya",
    "にゅ" -> "nyu",
    "にょ" -> "nyo",
    "な" -> "na",
    "に" -> "ni",
    "ぬ" -> "nu",
    "ね" -> "ne",
    "の" -> "no",
    "ひゃ" -> "hya",
    "ひゅ" -> "hyu",
    "ひょ" -> "hyo",
    "びゃ" -> "bya",
    "びゅ" -> "byu",
    "びょ" -> "byo",
    "ぴゃ" -> "pya",
    "ぴゅ" -> "pyu",
    "ぴょ" -> "pyo",
    "は" -> "ha",
    "ひ" -> "hi",
    "ふ" -> "fu",
    "へ" -> "he",
    "ほ" -> "ho",
    "ば" -> "ba",
    "び" -> "bi",
    "ぶ" -> "bu",
    "べ" -> "be",
    "ぼ" -> "bo",
    "ぱ" -> "pa",
    "っぱ" -> "ppa",
    "ぴ" -> "pi",
    "っぴ" -> "ppi",
    "ぷ" -> "pu",
    "っぷ" -> "ppu",
    "ぺ" -> "pe",
    "っぺ" -> "ppe",
    "ぽ" -> "po",
    "っぽ" -> "ppo",
    "ま" -> "ma",
    "み" -> "mi",
    "む" -> "mu",
    "め" -> "me",
    "も" -> "mo",
    "や" -> "ya",
    "ゆ" -> "yu",
    "よ" -> "yo",
    "りゃ" -> "rya",
    "りゅ" -> "ryu",
    "りょ" -> "ryo",
    "ら" -> "ra",
    "り" -> "ri",
    "る" -> "ru",
    "れ" -> "re",
    "ろ" -> "ro",
    "わ" -> "wa",
    "を" -> "wo",
    "ん" -> "n"
  )

  private def normalisedTextIteration(text: String, acc: String): Option[String] = {
    val matching = hiraganaConversions.find{ case (t,_) => text.startsWith(t) }
    matching.flatMap { case (kana, roman) =>
      val newText = text.substring(kana.length)
      val newAcc = acc + roman
      if (newText == "") Some(newAcc)
      else normalisedTextIteration(newText, newAcc)
    }
  }

  /**
   * Retrieves a normalised that can be used to match search patterns for this word
   */
  def normalisedText(text: String) = normalisedTextIteration(text, "")
}
