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

  private def isReferenced(registerDefinition: RegisterDefinition, key: Register.Key): Boolean = {
    val referencers = reverseReferences.getOrElse(registerDefinition, Nil)
    if (referencers.nonEmpty) {
      val references = for {
        sourceRegDef <- referencers
        table <- tables.get(sourceRegDef)
      } yield {
        table.exists { case (_,r) =>
          r.fields.collect { case field :ForeignKeyField => field }
            .exists(field => field.definition.target == registerDefinition && field.key == key)
        }
      }

      references.exists(x => x)
    }
    else false
  }

  override def delete(registerDefinition: RegisterDefinition, key: Register.Key): Boolean = {
    println(s"Called delete: isReferenced returned ${isReferenced(registerDefinition, key)}")

    !isReferenced(registerDefinition, key) &&
      tables.get(registerDefinition).flatMap(_.remove(key)).isDefined
  }
}
