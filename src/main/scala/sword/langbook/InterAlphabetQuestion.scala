package sword.langbook

import sword.langbook.db.{LinkedStorageManager, Word, Alphabet}

import scala.util.Random

/**
  * ViewModel for an inter-alphabet question.
  *
  * This class implementation assumes that the word passed contains all demanded alphabets.
  * It is recommended to use the companion object methods to generate an instance of this class.
  *
  * @param word Word to be asked
  * @param sourceAlphabets alphabets that will be displayed as question/clues.
  * @param targetAlphabets expected answers.
  */
class InterAlphabetQuestion(
    val word: Word, val sourceAlphabets: Set[Alphabet], val targetAlphabets: Set[Alphabet])
    extends Question {

  private def extractAlphabets(alphabets: Set[Alphabet]) = {
    alphabets.map(alphabet => (alphabet, word.text(alphabet))).toMap
  }

  def expectedAnswer: Map[Alphabet, String] = extractAlphabets(targetAlphabets)

  override val clues: Map[Alphabet, String] = extractAlphabets(sourceAlphabets)
  override val possibleAnswers = Set(expectedAnswer)

  override def encoded: String = {
    val sources = sourceAlphabets.map(_.key.encoded).mkString(",")
    val targets = targetAlphabets.map(_.key.encoded).mkString(",")
    s"${word.key.encoded};$sources;$targets"
  }
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

  def decode(manager: LinkedStorageManager, encodedQuestion: String) = {
    try {
      val storageManager = manager.storageManager
      val array = encodedQuestion.split(";")
      if (array.size == 3) {
        val wordOption = storageManager.decode(array.head).map(key => Word(key))
        val sources = array(1).split(",").flatMap(manager.storageManager.decode)
          .map(key => Alphabet(key)).toSet
        val targets = array(2).split(",").flatMap(manager.storageManager.decode)
          .map(key => Alphabet(key)).toSet
        wordOption.map(word => new InterAlphabetQuestion(word, sources, targets))
      }
      else None
    }
    catch {
      case _: ArrayIndexOutOfBoundsException => None
    }
  }
}
