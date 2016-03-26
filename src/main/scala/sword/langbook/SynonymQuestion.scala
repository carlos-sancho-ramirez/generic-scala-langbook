package sword.langbook

import sword.langbook.db.{Concept, LinkedStorageManager, Alphabet, Word}

import scala.util.Random

/**
  * View model for question where synonyms of a word are asked.
  *
  * As a word can have more than one synonym, there can be more than a possible answer.
  * All valid answers are listed in the {@link #possibleAnswers} set.
  *
  * This class expects that both question and answers will contain at least the alphabet provided.
  * It is recommended to use the companion object method to construct an instance of this that may
  * satisfy this assumption.
  *
  * @param concept Concept that must be shared among words, making them synonyms.
  * @param sourceWord Word in the question.
  * @param alphabet Alphabet to be used to display all words.
  */
class SynonymQuestion( val concept: Concept, val sourceWord: Word, val alphabet: Alphabet) {
  def clues: String = sourceWord.text(alphabet)
  def possibleAnswers: Set[String] = {
    sourceWord.synonyms.filter(_.concepts.contains(concept)).map(_.text(alphabet)).toSet
  }
}

object SynonymQuestion {
  def newAleatoryQuestion(manager: LinkedStorageManager, alphabet: Alphabet): Option[SynonymQuestion] = {
    val allPossibilities = manager.words.values.flatMap { word =>
      if (word.text.keySet.contains(alphabet)) {
        word.concepts.filter { concept =>
          concept.wordsForLanguage(word.language).filter { w =>
            w != word && w.text.keySet.contains(alphabet)
          }.nonEmpty
        }.map((_, word))
      }
      else None
    }.toSet.toVector

    if (allPossibilities.nonEmpty) {
      val (concept, word) = allPossibilities(Random.nextInt(allPossibilities.size))
      Some(new SynonymQuestion(concept, word, alphabet))
    }
    else None
  }
}
