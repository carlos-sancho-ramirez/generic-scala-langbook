package sword.langbook

import sword.langbook.db.{LinkedStorageManager, Alphabet}

/**
 * Root point for all questions.
 *
 * This trait will be used by the UI to display in a generic way one or another question
 */
trait Question {
  def clues: Map[Alphabet, String]
  def possibleAnswers: Set[Map[Alphabet, String]]

  /**
   * Returns an string that can be used to create this question again.
   */
  def encoded: String
}

object Question {
  object questionTypes {
    val interAlphabet = 'A'
    val synonym = 'S'
    val translation = 'T'
  }

  def decode(manager: LinkedStorageManager, encodedQuestion: String): Option[Question] = {
    val questionType = encodedQuestion.head
    val rest = encodedQuestion.tail

    questionType match {
      case questionTypes.interAlphabet => InterAlphabetQuestion.decode(manager, rest)
      case questionTypes.synonym => SynonymQuestion.decode(manager, rest)
      case questionTypes.translation => TranslationQuestion.decode(manager, rest)
      case _ => None
    }
  }
}