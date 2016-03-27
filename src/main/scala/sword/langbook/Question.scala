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
   * The result of this string do not include the type of question.
   */
  protected def encoded: String

  /**
   * Encodes the question including the type as well.
   */
  def encodedQuestion: String = {
    this match {
      case _: InterAlphabetQuestion => s"${Question.questionTypes.interAlphabet}$encoded"
      case _: SynonymQuestion => s"${Question.questionTypes.synonym}$encoded"
      case _: TranslationQuestion => s"${Question.questionTypes.translation}$encoded"
    }
  }
}

object Question {
  object questionTypes {
    val interAlphabet = 'A'
    val synonym = 'S'
    val translation = 'T'
  }

  /**
    * Decodes a question.
    * @param manager LinkedStorageManager to be used to generate the question again.
    * @param encodedQuestion String returned by {@link Question#encodedQuestion}
    * @return an instance of the question
    */
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