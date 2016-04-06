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
class SynonymQuestion(val concept: Concept, val sourceWord: Word, val alphabet: Alphabet) extends Question {
  def clue: String = sourceWord.text(alphabet)
  override val possibleAnswers: Set[Map[Alphabet, String]] = {
    sourceWord.synonyms.filter(_.concepts.contains(concept)).map(word => Map(alphabet -> word.text(alphabet))).toSet
  }

  override val clues: Map[Alphabet, String] = Map(alphabet -> clue)

  override def encoded = s"${concept.key.encoded};${sourceWord.key.encoded};${alphabet.key.encoded}"
}

object SynonymQuestion {
  def newAleatoryQuestion(alphabet: Alphabet)(manager: LinkedStorageManager): Option[SynonymQuestion] = {
    val allPossibilities = manager.words.values.flatMap { word =>
      if (word.text.keySet.contains(alphabet)) {
        word.concepts.filter { concept =>
          concept.wordsForLanguage(word.language).exists { w =>
            w != word && w.text.keySet.contains(alphabet)
          }
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

  /**
   * Checks in the database all synonyms that share the same alphabet.
   * It is expected that {@link #newAleatoryQuestion} will never return null for all alphabets
   * returned here.
   * @param manager LinkedStorageManager used to check the database.
   * @return A Set of alphabets that can be used to generate valid questions.
   *         This may be empty if no question can be created at all with the current database state.
   */
  def findPossibleQuestionTypes(manager: LinkedStorageManager): Set[Alphabet] = {

    val result = scala.collection.mutable.Set[Alphabet]()
    for {
      concept <- manager.concepts.values
    } {
      val semiResult = scala.collection.mutable.Set[Alphabet]()
      for {
        word <- concept.words
        alphabet <- word.text.keySet
      } {
        if (semiResult.contains(alphabet)) result += alphabet
        else semiResult += alphabet
      }
    }

    result.toSet
  }

  def decode(manager: LinkedStorageManager, encodedQuestion: String) = {
    try {
      val storageManager = manager.storageManager
      val array = encodedQuestion.split(";")
      if (array.size == 3) {
        val conceptOption = storageManager.decode(array.head).map(key => Concept(key))
        val sourceWordOption = storageManager.decode(array(1)).map(key => Word(key))
        val alphabetOption = storageManager.decode(array(2)).map(key => Alphabet(key))

        if (conceptOption.isDefined && sourceWordOption.isDefined && alphabetOption.isDefined) {
          Some(new SynonymQuestion(conceptOption.get, sourceWordOption.get, alphabetOption.get))
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
