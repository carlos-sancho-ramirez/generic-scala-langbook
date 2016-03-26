package sword.langbook

import scala.util.Random
import sword.langbook.db._

/**
  * View holder for a translation question
  *
  * @param concept Concept that must be shared among words, saying that way that they mean the same.
  * @param sourceWord Word to display as question.
  * @param targetLanguage Language expected as answer, this should not match the language for the sourceWord.
  * @param sourceAlphabets alphabets to be displayed in the question. sourceWord should have all this alphabets defined.
  * @param targetAlphabets All alphabets expected in the answer. This alfabet should be one of the languages used in the targetLanguage.
  */
class TranslationQuestion(val concept: Concept,
  val sourceWord: Word, val targetLanguage: Language,
  val sourceAlphabets: Set[Alphabet], val targetAlphabets: Set[Alphabet]) extends Question {

  override val clues: Map[Alphabet, String] = sourceAlphabets.map(alphabet => (alphabet, sourceWord.text(alphabet))).toMap
  override val possibleAnswers = {
    concept.wordsForLanguage(targetLanguage).filter { word =>
      targetAlphabets.diff(word.text.keySet).isEmpty
    }.map(word => targetAlphabets.map(alphabet => (alphabet, word.text(alphabet))).toMap).toSet
  }

  override def encoded: String = {
    val sources = sourceAlphabets.map(_.key.encoded).mkString(",")
    val targets = targetAlphabets.map(_.key.encoded).mkString(",")
    s"${concept.key.encoded};${sourceWord.key.encoded};${targetLanguage.key.encoded};$sources;$targets"
  }
}

object TranslationQuestion {
  def newAleatoryQuestion(manager: LinkedStorageManager,
      sourceLanguage: Language, targetLanguage: Language,
      sourceAlphabets: Set[Alphabet], targetAlphabets: Set[Alphabet]): Option[TranslationQuestion] = {
    if (sourceLanguage != targetLanguage) {
      val possibilities = manager.concepts.values.flatMap { concept =>
        val validTarget = concept.wordsForLanguage(targetLanguage).exists { word =>
          targetAlphabets.diff(word.text.keySet).isEmpty
        }

        if (validTarget) {
          concept.wordsForLanguage(sourceLanguage).flatMap { word =>
            if (sourceAlphabets.diff(word.text.keySet).isEmpty) {
              Some((concept, word))
            }
            else None
          }
        }
        else None
      }.toSet.toVector

      if (possibilities.nonEmpty) {
        val (concept, sourceWord) = possibilities(Random.nextInt(possibilities.size))
        Some(new TranslationQuestion(concept, sourceWord, targetLanguage, sourceAlphabets,
          targetAlphabets))
      }
      else None
    }
    else None
  }

  def decode(manager: LinkedStorageManager, encodedQuestion: String) = {
    try {
      val storageManager = manager.storageManager
      val array = encodedQuestion.split(";")
      if (array.size == 5) {
        val conceptOption = storageManager.decode(array.head).map(key => Concept(key))
        val sourceWordOption = storageManager.decode(array(1)).map(key => Word(key))
        val targetLanguageOption = storageManager.decode(array(2)).map(key => Language(key))
        val sources = array(3).split(",").flatMap(manager.storageManager.decode)
          .map(key => Alphabet(key)).toSet
        val targets = array(4).split(",").flatMap(manager.storageManager.decode)
          .map(key => Alphabet(key)).toSet

        if (conceptOption.isDefined && sourceWordOption.isDefined && targetLanguageOption.isDefined) {
          Some(new TranslationQuestion(conceptOption.get, sourceWordOption.get,
            targetLanguageOption.get, sources, targets))
        }
        else None
      }
      else None
    }
    catch {
      case _: ArrayIndexOutOfBoundsException => None
    }
  }
}
