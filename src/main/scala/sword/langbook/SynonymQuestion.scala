package sword.langbook

import sword.db.ForeignKeyField
import sword.langbook.db.registers.{PieceArrayReferenceField, PieceReferenceField, AlphabetReferenceField}
import sword.langbook.db._

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

    val storageManager = manager.storageManager

    val wordConceptRelations = storageManager.getMapFor(registers.WordConcept).values.map(reg => (reg.concept, reg.word))

    val result = scala.collection.mutable.Set[Alphabet]()
    for (alphabetKey <- storageManager.getKeysFor(registers.Alphabet)) {
      val pieces = storageManager.getMapFor(registers.Piece, AlphabetReferenceField(alphabetKey)).map(_._1.group)
      val pieceArrays = pieces.flatMap {
        piece =>
          storageManager.getMapFor(registers.PiecePosition, PieceReferenceField(piece)).map(_._1.group)
      }.toSet

      // We are assuming here that all words are from the same language. This is OK if the given
      // alphabet is only defined for that language.
      // TODO: Return language as well in the method assuming that the same alphabet can be used
      // across different languages.
      val wordKeys = pieceArrays.flatMap(array => storageManager.getKeysFor(registers.Word, PieceArrayReferenceField(array)))

      if (wordKeys.exists {
        wordKey =>
          val conceptKeys = wordConceptRelations.filter(_._2 == wordKey).map(_._1)
          conceptKeys.exists {
            conceptKey =>
              var foundOnce = false
              wordConceptRelations.exists { t =>
                if (t._1 == conceptKey && wordKeys.contains(t._2)) {
                  if (foundOnce) true
                  else {
                    foundOnce = true
                    false
                  }
                }
                else false
              }
          }
      }) {
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
