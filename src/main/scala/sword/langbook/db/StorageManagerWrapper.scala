package sword.langbook.db

/**
 * Wraps an storage manager delegating all to the wrapped instance
 */
trait StorageManagerWrapper extends StorageManager {
  def wrapped :StorageManager

  override def registerDefinitions = wrapped.registerDefinitions
  override def insert(register :Register) = wrapped.insert(register)
  override def delete(registerDefinition: RegisterDefinition, key :Register.Key) = wrapped.delete(registerDefinition, key)
  override def get(registerDefinition :RegisterDefinition, key :Register.Key) = wrapped.get(registerDefinition, key)
  override def getKeysFor(registerDefinition :RegisterDefinition) = wrapped.getKeysFor(registerDefinition)
  override def replace(register :Register, key :Register.Key) = replace(register, key)
}
