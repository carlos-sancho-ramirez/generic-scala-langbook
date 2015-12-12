package sword.langbook.db

class StorageManager(val registerDefinitions :Seq[RegisterDefinition]) {

  /**
   * Add a new register.
   * @return the assigned primary key
   */
  def insert(register :Register) :Register.Key = ???

  /**
   * Removes the register with the given key and definition if it exists and it's possible.
   * @param key Primary key for the register to remove, the one returned by insert method when added.
   * @param registerDefinition Definition for the register to be removed.
   * @return Some instance containing the removed register data, or None if not removed.
   */
  def delete(key :Register.Key, registerDefinition: RegisterDefinition) :Option[Register] = ???

  /**
   * Retrieves the register that matches the given key and definition.
   * @return A Some instance with the register instance inside of None if not found.
   */
  def get(key :Register.Key, registerDefinition :RegisterDefinition) :Option[Register] = ???
}
