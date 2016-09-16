package sword.langbook

import sword.db.StorageManager
import sword.langbook.db._
import sword.langbook.db.registers.{WordReferenceFieldDefinition, AlphabetReferenceField, WordConcept, WordRepresentation}

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
    val wordConcepts = manager.storageManager.getJointSet(WordRepresentation, WordConcept,
        AlphabetReferenceField(alphabet.key), WordReferenceFieldDefinition)

    val possibleWords = wordConcepts.groupBy(_.concept.index).values.foldLeft(Set[WordConcept]()) {
      (result, set) =>
        if (set.size > 1) result ++ set
        else result
    }.toVector

    if (possibleWords.nonEmpty) {
      val wordConcept = possibleWords(Random.nextInt(possibleWords.size))
      Some(new SynonymQuestion(Concept(wordConcept.concept), Word(wordConcept.word), alphabet))
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
    val storageManager = manager.storageManager
    val result = scala.collection.mutable.Set[Alphabet]()
    for (alphabetKey <- storageManager.getKeysFor(registers.Alphabet)) {
      if (storageManager.isConceptDuplicated(alphabetKey)) {
        result += Alphabet(alphabetKey)
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
