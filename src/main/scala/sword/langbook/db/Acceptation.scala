package sword.langbook.db

import sword.db.StorageManager

case class Acceptation(key: StorageManager.Key) {
  private def storageManager = key.storageManager
  private def accRegOption = storageManager.get(key).map(_.asInstanceOf[registers.Acceptation])

  def concept: Concept = Concept(accRegOption.get.concept)
  def word: Word = Word(accRegOption.get.word)

  /**
   * Return a representation attached to this acceptation. In case of exists more than one,
   * only one will be returned.
   * This will return None if the acceptation has no representation but the word does.
   * In order to a suitable one from acceptation or word, call suitableRepresentation method instead.
   */
  def anyAcceptationRepresentation: Option[String] = {
    val regDef = registers.AcceptationRepresentation
    val field = regDef.AcceptationReferenceField(key)
    storageManager.getJointSet(regDef, redundant.Text, field, regDef.SymbolArrayReferenceField, redundant.Text.SymbolArrayReferenceField)
      .map(_.text).headOption
  }

  def suitableText: Option[String] = {
    val accRepr = anyAcceptationRepresentation
    if (anyAcceptationRepresentation.isDefined) accRepr
    else accRegOption.flatMap(accReg => Word(accReg.word).suitableText)
  }

  private def otherAcceptationsWithSameConcept = {
    val regOption = accRegOption
    if (regOption.isDefined) {
      val accReg = accRegOption.get
      val regDef = registers.Acceptation
      val filter = regDef.ConceptReferenceField(accReg.concept)
      storageManager.getMapFor(regDef, filter).collect {
        case (accKey, acc) if acc.word != accReg.word => Acceptation(accKey)
      }
    }
    else Set()
  }

  def synonyms = {
    val thisLanguage = word.language
    otherAcceptationsWithSameConcept.filter(_.word.language == thisLanguage)
  }

  def translations = {
    val thisLanguage = word.language
    otherAcceptationsWithSameConcept.filter(_.word.language != thisLanguage)
  }
}

object Acceptation extends ElementFactory[registers.Acceptation, Acceptation] {
  def from(manager: LinkedStorageManager, word: Word, concept: Concept): Option[Acceptation] = {
    from(manager, registers.Acceptation(word.key, concept.key))
  }
}
