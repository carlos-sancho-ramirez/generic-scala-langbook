package sword.db

object StorageManager {

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
      val registerDefinition :RegisterDefinition,
      val group :Register.CollectionId,
      val index :Register.Index) {
    def registerOption = storageManager.get(this)

    override def toString = s"$group:$index"

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

  protected def obtainKey(registerDefinition :RegisterDefinition, group :Register.CollectionId,
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
  def registerDefinitions :Seq[RegisterDefinition]

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
  def getKeysFor(registerDefinition :RegisterDefinition) :Set[Key]

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
  def getMapFor(registerDefinition :RegisterDefinition) :scala.collection.Map[Key, Register] = {
    getKeysFor(registerDefinition).groupBy(x => x).map { case (key, _) =>
      (key, get(key).get)
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
  def getKeysForCollection(registerDefinition :CollectibleRegisterDefinition, id :Register.CollectionId) :Set[Key]

  /**
   * Returns a map matching keys with their registers for all registers within the given collection
   *
   * @param registerDefinition Kind of register to be retrieved
   * @param id Identifier for the collection
   */
  def getMapForCollection(registerDefinition :CollectibleRegisterDefinition, id :Register.CollectionId) :scala.collection.Map[Key, Register]

  /**
   * Returns all keys matching registers that contains the given collection identifier in the expected order.
   *
   * @param registerDefinition Identifier. As collection identifiers can only be added in just one
   *                           registerDefinition, this also identifies the register definition
   *                           where this is included.
   * @param id identifier value to be filtered
   */
  def getKeysForArray(registerDefinition :ArrayableRegisterDefinition, id :Register.CollectionId) :Seq[Key] = {
    getKeysForCollection(registerDefinition, id).toSeq.sortBy(_.index)
  }
}
