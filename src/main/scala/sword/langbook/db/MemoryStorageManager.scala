package sword.langbook.db

class MemoryStorageManager(registerDefinitions :Seq[RegisterDefinition]) extends StorageManager(registerDefinitions) {

  private val tables :Map[RegisterDefinition, scala.collection.mutable.Map[Register.Key, Register]] =
      registerDefinitions.map(d => (d,scala.collection.mutable.Map[Register.Key, Register]())).toMap
  private var lastKey :Register.Key = 0

  override def insert(register: Register): Option[Register.Key] = {
    tables.get(register.definition).map { map =>
      lastKey += 1
      map.put(lastKey, register)
      lastKey
    }
  }

  /**
   * Retrieves the register that matches the given key and definition.
   * @return A Some instance with the register instance inside of None if not found.
   */
  override def get(key: Register.Key, registerDefinition: RegisterDefinition): Option[Register] = {
    tables.get(registerDefinition).flatMap(_.get(key))
  }

  /**
   * Removes the register with the given key and definition if it exists and it's possible.
   * @param key Primary key for the register to remove, the one returned by insert method when added.
   * @param registerDefinition Definition for the register to be removed.
   * @return Some instance containing the removed register data, or None if not removed.
   */
  override def delete(key: Register.Key, registerDefinition: RegisterDefinition): Option[Register] = ???
}
