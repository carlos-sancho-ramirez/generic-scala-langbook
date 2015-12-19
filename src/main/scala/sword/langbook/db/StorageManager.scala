package sword.langbook.db

abstract class StorageManager(val registerDefinitions :Seq[RegisterDefinition]) {

  if (registerDefinitions.toSet.size < registerDefinitions.size) {
    throw new IllegalArgumentException("Duplicated register definitions are not allowed")
  }

  /**
   * List all connections between register definitions, being the first the source and the second
   * the target definition.
   */
  val references :Seq[(RegisterDefinition, RegisterDefinition)] = for {
    regDef <- registerDefinitions
    fieldDef <- regDef.fields if fieldDef.isInstanceOf[ForeignKeyFieldDefinition]
  } yield {
    // TODO: Check that target is included in registerDefinitions
    (regDef, fieldDef.asInstanceOf[ForeignKeyFieldDefinition].target)
  }

  if (references.exists { case (_,target) => !registerDefinitions.contains(target) }) {
    throw new IllegalArgumentException("All given register definitions that include a foreign key" +
        " field must have as target one of the definitions given")
  }

  val reverseReferences :Map[RegisterDefinition,Seq[RegisterDefinition]] =
      references.groupBy{ case (s,t) => t}.map { case (t, seq) => (t, seq.map(_._1))}

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
}
