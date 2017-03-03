package sword.db

import sword.langbook.db.registers
import sword.langbook.db.registers.WordReferenceFieldDefinition

object StorageManager {

  // This is required in order that app can identify languages when selecting quizzes.
  // It should not be required if the app was generic enough.
  // TODO: Remove these codes when the app does not require them
  object LanguageCodes {
    val english = "en"
    val spanish = "es"
    val japanese = "ja"
  }

  /**
   * Key that can be used to identify a single or a collection of registers within a storage manager.
   *
   * Keys are expected to be created by the storage manager implementation and never by the client
   * of the storage manager implementation.
   *
   * @param registerDefinition definition of the register that this key is pointing to
   * @param group identifier for a collection of registers
   * @param index identifier to identify a single register within a collection
   */
  sealed class Key private[StorageManager] (
      val storageManager :StorageManager,
      val registerDefinition :RegisterDefinition[Register],
      val group :Register.CollectionId,
      val index :Register.Index) {
    def registerOption = storageManager.get(this)
    def delete = storageManager.delete(this)

    override def toString = {
      val regDefIndex = storageManager.registerDefinitions.indexOf(registerDefinition)
      s"$regDefIndex:$group:$index"
    }

    /**
     * Encodes this key.
     */
    def encoded = storageManager.encode(this)
    override def equals(other: Any) = other.isInstanceOf[Key] && other.asInstanceOf[Key].encoded == encoded
    override def hashCode = group * 31 + index
  }
}

/**
 * Trait for basic relational storage.
 */
trait StorageManager {

  type Key = StorageManager.Key

  protected def obtainKey(registerDefinition :RegisterDefinition[Register], group :Register.CollectionId,
    index :Register.Index) = new Key(this, registerDefinition, group, index)

  /**
   * Transforms the given key into an string that can be used to serialize it and reconstruct it
   * again later. This is valuable for those situations where the key instance cannot be kept.
   *
   * Some concrete scenarios are:
   *   * Web: when the key has to be send as GET parameter
   *   * Android: when the key has to go across process (Inter-process communication) and has to
   *     be added in a Parcel.
   *
   * Every StorageManager implementation can create its own implementation in order to protect
   * any confidential info.
   *
   * It is assumed that any instance of the same storage manager implementation can decode again
   * keys if they have the same register definitions and in the same order.
   */
  def encode(key: Key): String = {
    val regDefIndex = registerDefinitions.indexOf(key.registerDefinition)
    s"$regDefIndex:${key.group}:${key.index}"
  }

  /**
   * Complementary process of encoding. This retrieves a key back that has been encoded by this
   * storage manager with the same configuration.
   *
   * @return A some option including the key or None in case of error
   */
  def decode(encodedKey: String): Option[Key] = {
    if (encodedKey != null) {
      try {
        val array = encodedKey.split(":").map(_.toInt)
        Some(obtainKey(registerDefinitions(array(0)), array(1), array(2)))
      }
      catch {
        case _: ArrayIndexOutOfBoundsException => None
        case _: NumberFormatException => None
      }
    }
    else None
  }

  /**
   * Sequence for all registerDefinitions that this store manager understand.
   *
   * It is expected not to have duplicated register definition within the sequence and being self-contained.
   * By self-contained it is understood to have all foreign keys and set reference pointing to
   * register definitions and fields included in this sequence.
   */
  def registerDefinitions :Seq[RegisterDefinition[Register]]

  /**
   * Add a new register.
   *
   * @return A Some instance with the assigned primary key inside or None in case of error
   */
  def insert(register :Register) :Option[Key]

  /**
   * Add a new collection of registers
   *
   * @param registers Registers to be added in the new collection.
   * @return A Some instance containing the given collection identifier or None if not possible
   */
  def insert(registers :Traversable[Register]) :Option[Register.CollectionId]

  /**
   * Inserts a register into a collection. In case it is an arrayable register, the given register
   * will be added at the end of the array.
   *
   * @param collectionId identifier for the collection where the register is going to be inserted.
   * @param register New register to be added. It must be collectible or arrayable.
   * @return A Some instance with the Key for the new register, or None if something went wrong.
   */
  def insert(collectionId :Register.CollectionId, register :Register) :Option[Key]

  /**
   * Removes the register with the given key and definition if it exists and it's possible.
   *
   * @param key Key for the register to remove, the one returned by insert method when added.
   * @return Whether it has been removed.
   */
  def delete(key :Key) :Boolean

  /**
   * Retrieves the register that matches the given key and definition.
   *
   * @return A Some instance with the register instance inside of None if not found.
   */
  def get(key :Key) :Option[Register]

  /**
   * Get all keys currently included in the given register definition.
   */
  def getKeysFor(registerDefinition :RegisterDefinition[Register]) :Set[Key]

  def getKeysFor(registerDefinition :RegisterDefinition[Register], filter: ForeignKeyField) :Set[Key] = {
    getMapFor(registerDefinition).flatMap {
      case (key, reg) =>
        reg.fields.collectFirst {
          case f: ForeignKeyField if f.definition.target == filter.definition.target && f.key == filter.key => key
        }
    }.toSet
  }

  def getKeysFor(registerDefinition :RegisterDefinition[Register], filter: CollectionReferenceField) :Set[Key] = {
    getMapFor(registerDefinition).flatMap {
      case (key, reg) =>
        reg.fields.collectFirst {
          case f: CollectionReferenceField if f.definition.target == filter.definition.target && f.collectionId == filter.collectionId => key
        }
    }.toSet
  }

  /**
   * Replace an already registered register content, mapped to the given key, with the new given values.
   *
   * @param register New values for the register
   * @param key A valid key for the register to replace.
   * @return true if all was fine, or false in case of any problem.
   */
  def replace(register :Register, key :Key) :Boolean

  /**
   * Returns a Map containing all values inserted for a given register definition.
   */
  def getMapFor[R <: Register](registerDefinition :RegisterDefinition[R]) :scala.collection.Map[Key, R] = {
    getKeysFor(registerDefinition).groupBy(x => x).map { case (key, _) =>
      (key, get(key).get.asInstanceOf[R])
    }
  }

  def getMapFor[R <: Register](registerDefinition: RegisterDefinition[R], filters: Field*): scala.collection.Map[Key, R] = {
    getMapFor(registerDefinition).filter {
      case (_, reg) =>
        val fields = reg.fields
        filters.forall(filter => fields.contains(filter))
    }
  }

  /**
   * Returns all keys matching registers that contains the given collection identifier.
   *
   * @param registerDefinition Identifier. As collection identifiers can only be added in just one
   *                           registerDefinition, this also identifies the register definition
   *                           where this is included.
   * @param id identifier value to be filtered
   */
  def getKeysForCollection(registerDefinition :CollectibleRegisterDefinition[Register], id :Register.CollectionId) :Set[Key]

  /**
   * Returns a map matching keys with their registers for all registers within the given collection
   *
   * @param registerDefinition Kind of register to be retrieved
   * @param id Identifier for the collection
   */
  def getMapForCollection[R <: Register](registerDefinition :CollectibleRegisterDefinition[R], id :Register.CollectionId) :scala.collection.Map[Key, R]

  /**
   * Returns all keys matching registers that contains the given collection identifier in the expected order.
   *
   * @param registerDefinition Identifier. As collection identifiers can only be added in just one
   *                           registerDefinition, this also identifies the register definition
   *                           where this is included.
   * @param id identifier value to be filtered
   */
  def getKeysForArray(registerDefinition: ArrayableRegisterDefinition[Register], id: Register.CollectionId) :Seq[Key] = {
    getKeysForCollection(registerDefinition, id).toSeq.sortBy(_.index)
  }

  def getCollection[R <: Register](registerDefinition: CollectibleRegisterDefinition[R], id: Register.CollectionId): Set[R] = {
    getMapForCollection(registerDefinition, id).values.toSet
  }

  /**
    * Returns an array of registers
    *
    * @param registerDefinition Type of the register to be retrieved. This must be one of the
    *                           definitions in {@link #registerDefinitions}
    * @param id Identifier for the array to be retrieved.
    * @tparam R Type of register to be retrieved.
    * @return an array of registers of the given type.
    */
  def getArray[R <: Register](registerDefinition: ArrayableRegisterDefinition[R], id: Register.CollectionId) :Seq[R] = {
    getKeysForArray(registerDefinition, id).flatMap(key => get(key)).asInstanceOf[Seq[R]]
  }

  /**
   * Trial to get all alphabets for a given language. This will not include preferred alphabet if
   * no word is defined for it.
    *
    * @param language key for the language
   * @return Set of alphabet keys
   */
  def getAlphabetSet(language: ForeignKeyField): Set[Key] = {
    val wordRefFieldDef = registers.WordRepresentation.WordReferenceField
    def keyExtractor(str: String) = {
      try {
        Some(obtainKey(wordRefFieldDef.target, 0, str.toInt))
      }
      catch {
        case _: NumberFormatException => None
      }
    }

    for {
      wordKey <- getKeysFor(registers.Word, language)
      wordRefField <- wordRefFieldDef.from(wordKey.index.toString, keyExtractor).toList
      repr <- getMapFor(registers.WordRepresentation, wordRefField).values
    } yield {
      repr.alphabet
    }
  }

  /**
   * Makes a join of 2 tables where the first one includes a foreign key of the second one.
   *
   * @param regDef Register definition for the first table
   * @param targetRegDef Register definition for the second table.
   *                     This is redundant as it can be extracted from the foreign key field,
   *                     but having this we can ensure type safety.
   * @param filter Field for the first table. This is used to filter the results
   * @param sourceJoinFieldDefinition: Field definition for the first table
   * @return A Map for all matching registers in the second table
   */
  def getForeignMap[R <: Register](
      regDef: RegisterDefinition[Register],
      targetRegDef: RegisterDefinition[R],
      sourceJoinFieldDefinition: ForeignKeyFieldDefinition,
      filters: Field*): scala.collection.Map[Key, R] = {

    val fieldIndex = regDef.fields.indexOf(sourceJoinFieldDefinition)
    getMapFor(regDef, filters: _*).map { pair =>
      val regKey = pair._2.fields(fieldIndex).asInstanceOf[ForeignKeyField].key
      val newReg = get(regKey).get.asInstanceOf[R]

      if (newReg.definition != targetRegDef) {
        throw new AssertionError("Wrong register definition")
      }

      (regKey, newReg)
    }
  }

  /**
   * Specific query that makes a join of 2 tables, uses a given filter for the first table and
   * returns completely the second table as a register.
   *
   * @param sourceRegDef First table to be used in a JOIN
   * @param targetRegDef Second table to be used in a JOIN
   * @param filter ForeignKeyField included in the first table. This is used to filter results and
   *               only take the ones matching the given key.
   * @param joinLeft FieldDefinition for the source table to match with joinRight.
   * @param joinRight FieldDefinition for the target table to match with joinLeft
   * @tparam R RegisterDefinition for the second table
   * @return A set of all registers in the second table that remained after filtering
   */
  def getJointSet[R <: Register](
      sourceRegDef: RegisterDefinition[Register],
      targetRegDef: RegisterDefinition[R],
      filter: ForeignKeyField,
      joinLeft: FieldDefinition,
      joinRight: FieldDefinition): Set[R] = Set()

  /**
   * Query and return all existing symbol arrays in the system.
   *
   * @return
   */
  def allStringArray: Map[Key, List[String]] = Map()

  /**
   * Retrieve a map containing all keys for words included in a given bunch as key,
   * and a list of all text combinations as value.
   */
  def allWordTexts(bunch: Key): Map[Key /* Word */, List[String]] = Map()

  /**
   * Trial to boost up the synonyms search
   */
  def isConceptDuplicated(alphabet: Key) = false

  /**
   * Find all alphabets containing the given symbol
   */
  def alphabetsWhereSymbolIncluded(symbol: Key): Set[Key] = Set()

  /**
   * Find all symbols contained in a given alphabet
   */
  def allSymbolsInAlphabet(alphabet: Key): Set[Key] = Set()
}
