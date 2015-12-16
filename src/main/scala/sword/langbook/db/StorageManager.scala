package sword.langbook.db

abstract class StorageManager(val registerDefinitions :Seq[RegisterDefinition]) {

  if (registerDefinitions.toSet.size < registerDefinitions.size) {
    throw new IllegalArgumentException("Duplicated register definitions are not allowed")
  }

  /**
   * Add a new register.
   * @return A Some instance with the assigned primary key inside or None in case of error
   */
  def insert(register :Register) :Option[Register.Key]

  /**
   * Removes the register with the given key and definition if it exists and it's possible.
   * @param registerDefinition Definition for the register to be removed.
   * @param key Primary key for the register to remove, the one returned by insert method when added.
   * @return Some instance containing the removed register data, or None if not removed.
   */
  def delete(registerDefinition: RegisterDefinition, key :Register.Key) :Option[Register]

  /**
   * Retrieves the register that matches the given key and definition.
   * @return A Some instance with the register instance inside of None if not found.
   */
  def get(registerDefinition :RegisterDefinition, key :Register.Key) :Option[Register]
}
