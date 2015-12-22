package sword.langbook.db

/**
 * Trait for basic relational storage.
 */
trait StorageManager {

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
  def insert(register :Register) :Option[Register.Key]

  /**
   * Removes the register with the given key and definition if it exists and it's possible.
   * @param registerDefinition Definition for the register to be removed.
   * @param key Primary key for the register to remove, the one returned by insert method when added.
   * @return Whether it has been removed.
   */
  def delete(registerDefinition: RegisterDefinition, key :Register.Key) :Boolean

  /**
   * Retrieves the register that matches the given key and definition.
   * @return A Some instance with the register instance inside of None if not found.
   */
  def get(registerDefinition :RegisterDefinition, key :Register.Key) :Option[Register]

  /**
   * Get all keys currently included in the given register definition.
   */
  def getKeysFor(registerDefinition :RegisterDefinition) :Set[Register.Key]

  /**
   * Replace an already registered register content, mapped to the given key, with the new given values.
   * @param register New values for the register
   * @param key A valid key for the register to replace.
   * @return true if all was fine, or false in case of any problem.
   */
  def replace(register :Register, key :Register.Key) :Boolean
}
