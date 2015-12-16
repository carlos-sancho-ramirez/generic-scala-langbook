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

  override def get(registerDefinition: RegisterDefinition, key: Register.Key): Option[Register] = {
    tables.get(registerDefinition).flatMap(_.get(key))
  }

  override def delete(registerDefinition: RegisterDefinition, key: Register.Key): Option[Register] = ???
}
