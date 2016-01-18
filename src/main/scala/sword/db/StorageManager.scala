package sword.db

object StorageManager {

  /**
   * Key that can be used to identify a single or a collection of registers within a storage manager.
   *
   * Keys are expected to be created by the storage manager implementation and never by the client
   * of the storage manager implementation.
   *
   * @param group identifier for a collection of registers
   * @param index identifier to identify a single register within a collection
   */
  sealed class Key private[StorageManager] (val group :Register.CollectionId, val index :Register.Index)
}

/**
 * Trait for basic relational storage.
 */
trait StorageManager {

  type Key = StorageManager.Key

  protected def obtainKey(group :Register.CollectionId, index :Register.Index) = new Key(group, index)

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
   * @return A Some instance with the assigned primary key inside or None in case of error
   */
  def insert(register :Register) :Option[Key]

  /**
   * Add a new collection of registers
   * @param registers Registers to be added in the new collection.
   * @return A Some instance containing the given collection identifier or None if not possible
   */
  def insert(registers :Traversable[Register]) :Option[Register.CollectionId]

  /**
   * Removes the register with the given key and definition if it exists and it's possible.
   * @param registerDefinition Definition for the register to be removed.
   * @param key Primary key for the register to remove, the one returned by insert method when added.
   * @return Whether it has been removed.
   */
  def delete(registerDefinition: RegisterDefinition, key :Key) :Boolean

  /**
   * Retrieves the register that matches the given key and definition.
   * @return A Some instance with the register instance inside of None if not found.
   */
  def get(registerDefinition :RegisterDefinition, key :Key) :Option[Register]

  /**
   * Get all keys currently included in the given register definition.
   */
  def getKeysFor(registerDefinition :RegisterDefinition) :Set[Key]

  /**
   * Replace an already registered register content, mapped to the given key, with the new given values.
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
      (key, get(registerDefinition, key).get)
    }
  }

  /**
   * Returns all keys matching registers that contains the given collection identifier.
   * @param registerDefinition Identifier. As collection identifiers can only be added in just one
   *                           registerDefinition, this also identifies the register definition
   *                           where this is included.
   * @param id identifier value to be filtered
   */
  def getKeysForCollection(registerDefinition :CollectibleRegisterDefinition, id :Register.CollectionId) :Set[Key]

  /**
   * Returns a map matching keys with their registers for all registers within the given collection
   * @param registerDefinition Kind of register to be retrieved
   * @param id Identifier for the collection
   */
  def getMapForCollection(registerDefinition :CollectibleRegisterDefinition, id :Register.CollectionId) :scala.collection.Map[Key, Register]
}
