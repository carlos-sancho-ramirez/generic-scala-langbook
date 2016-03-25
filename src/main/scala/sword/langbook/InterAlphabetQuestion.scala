package sword.langbook

import sword.langbook.db.{LinkedStorageManager, Word, Alphabet}

import scala.util.Random

/**
  * ViewModel for an inter-alphabet question.
  *
  * This class implementation assumes that the word passed contains all demanded alphabets.
  * It is recommended to use the companion object methods to generate an instance of this class.
  * @param word Word to be asked
  * @param sourceAlphabets alphabets that will be displayed as question/clues.
  * @param targetAlphabets expected answers.
  */
class InterAlphabetQuestion (
    val word: Word, val sourceAlphabets: Set[Alphabet], val targetAlphabets: Set[Alphabet]) {

  private def extractAlphabets(alphabets: Set[Alphabet]) = {
    alphabets.map(alphabet => (alphabet, word.text(alphabet))).toMap
  }

  def clues: Map[Alphabet, String] = extractAlphabets(sourceAlphabets)
  def expectedAnswer: Map[Alphabet, String] = extractAlphabets(targetAlphabets)
}

object InterAlphabetQuestion {
  def newAleatoryQuestion(manager: LinkedStorageManager, sourceAlphabets: Set[Alphabet],
      targetAlphabets: Set[Alphabet]): Option[InterAlphabetQuestion] = {
    val allAlphabets = sourceAlphabets ++ targetAlphabets
    if (allAlphabets.size == sourceAlphabets.size + targetAlphabets.size) {
      val words = manager.words.collect {
        case (_, word) if allAlphabets.diff(word.text.keySet).isEmpty => word
      }.toVector

      val randomWord = words(Random.nextInt(words.size))
      Some(new InterAlphabetQuestion(randomWord, sourceAlphabets, targetAlphabets))
    }
    else None
  }
}

/*
case class SynonymQuestion(val concept: Concept, val sourceWord: Word, val sourceAlphabets: Set[Alphabet], val targetAlphabets: Set[Alphabet]) {
  def clues: Map[Alphabet, String]
  def expectedAnswer: Set[Map[Alphabet, String]]
}

case class TranslationQuestion(val concept: Concept, val sourceLanguage: Language, val targetLanguage: Language, val sourceAlphabets: Set[Alphabet], val targetAlphabets: Set[Alphabet]) {
  def clues: Map[Alphabet, String]
  def expectedAnswer: Set[Map[Alphabet, String]]
}
*/