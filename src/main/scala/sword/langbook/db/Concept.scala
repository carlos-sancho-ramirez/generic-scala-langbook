package sword.langbook.db

import sword.db.{ForeignKeyField, StorageManager}

case class Concept(key: StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def hintOpt = fields.collectFirst {
    case field :registers.Concept.CharSequenceField => field.value
  }

  def hint = hintOpt.get

  def wordRegisterMap = key.storageManager
      .getMapFor(registers.Acceptation, registers.Acceptation.ConceptReferenceField(key))

  def words = wordRegisterMap.map(pair => Word(pair._2.word))

  def wordMap: scala.collection.Map[StorageManager.Key, registers.Word] = {
    val filter = registers.Acceptation.ConceptReferenceField(key)
    val foreignKeyFieldDef = registers.Acceptation.WordReferenceField
    key.storageManager.getForeignMap(registers.Acceptation, registers.Word, filter, foreignKeyFieldDef)
  }

  /**
   * Return an iterable collection for all words linked to this concept that also belong
   * to the given language, or an empty collection if none.
   */
  def wordsForLanguage(language :Language) = wordMap.collect {
    case (regKey, reg: registers.Word) if reg.language == language.key =>
      Word(regKey)
  }

  /**
   * Return word representing this concept. First this method tries to return a word for
   * the given language. If none, any from any other language. If no word is linked to
   * this concept, None will be returned.
   */
  def wordForLanguageOrFirst(language: Language): Option[Word] = {
    var wordKey: StorageManager.Key = null
    wordMap.exists { pair =>
      wordKey = pair._1
      pair._2.language == language.key
    }

    if (wordKey != null) Some(Word(wordKey))
    else None
  }

  def types = {
    key.storageManager.getMapFor(registers.ConceptTypeRelation).flatMap {
      case (_, reg) =>
        if (reg.fields(1).asInstanceOf[ForeignKeyField].key == key) {
          Some(Concept(reg.fields.head.asInstanceOf[ForeignKeyField].key))
        }
        else None
    }
  }.toSet

  def isTypeOf = {
    key.storageManager.getMapFor(registers.ConceptTypeRelation).flatMap {
      case (_, reg) =>
        if (reg.fields.head.asInstanceOf[ForeignKeyField].key == key) {
          Some(Concept(reg.fields(1).asInstanceOf[ForeignKeyField].key))
        }
        else None
    }
  }.toSet
}

object Concept extends ElementFactory[registers.Concept, Concept] {
  def from(manager: LinkedStorageManager, hint: String): Option[Concept] = {
    from(manager, registers.Concept(hint))
  }
}
