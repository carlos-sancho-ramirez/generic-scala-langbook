package sword.langbook

import sword.db.StorageManager
import sword.langbook.db.registers.WordRepresentation
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
  def newAleatoryQuestion(sourceAlphabets: Set[Alphabet], targetAlphabets: Set[Alphabet])(
      manager: LinkedStorageManager): Option[InterAlphabetQuestion] = {

    val allAlphabets = (sourceAlphabets ++ targetAlphabets).map(_.key).toList
    val alphabetCount = allAlphabets.size
    if (alphabetCount == sourceAlphabets.size + targetAlphabets.size && alphabetCount >= 2) {
      def wordsForAlphabet(alphabetKey: StorageManager.Key) = {
        manager.storageManager.getMapFor(WordRepresentation,
          WordRepresentation.AlphabetReferenceField(alphabetKey)).map { case (_, repr) => repr.word }.toSet
      }

      val possibleWords = allAlphabets.tail.foldLeft(wordsForAlphabet(allAlphabets.head)) {
        (result, alphabetKey) => result.union(wordsForAlphabet(alphabetKey))
      }.toVector

      val randomWord = possibleWords(Random.nextInt(possibleWords.size))
      Some(new InterAlphabetQuestion(Word(randomWord), sourceAlphabets, targetAlphabets))
    }
    else None
  }

  /**
    * Find all different combinations of source/target alphabets for the current state of the database.
    * The result of this method ensures that at least on e question can be created with the given
    * source/target alphabets when calling {@link #newAleatoryQuestion}
    * @param manager LinkedStorageManager to check the current database state
    * @return A set of pairs of source/target alphabets sets. This set will be empty if no
    *         InterAlphabetQuestion can be created at all. This can perfectly happen if there is no
    *         language in the database with at least 2 alphabets.
    */
  def findPossibleQuestionTypes(manager: LinkedStorageManager): Set[(Set[Alphabet], Set[Alphabet])] = {
    val result = scala.collection.mutable.Set[(Set[Alphabet], Set[Alphabet])]()
    for {
      language <- manager.languages.values
    } {
      val alphabets = language.alphabets.toList
      if (alphabets.size > 1) {
        for {
          permutation <- alphabets.permutations
        } {
          result += ((Set(permutation.head), Set(permutation(1))))
        }
      }
    }

    result.toSet
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
