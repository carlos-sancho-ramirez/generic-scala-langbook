package sword.langbook.db

import sword.langbook.db.Register.Key

/**
 * Implementation for StorageManager that saves all its data in memory.
 * This is expected to be faster than other implementations but all data will be lost whenever this
 * class is garbage collected.
 */
class MemoryStorageManager(registerDefinitions :Seq[RegisterDefinition]) extends AbstractStorageManager(registerDefinitions) {

  private val tables :Map[RegisterDefinition, scala.collection.mutable.Map[Register.Key, Register]] =
      registerDefinitions.map(d => (d,scala.collection.mutable.Map[Register.Key, Register]())).toMap
  private var lastKey :Register.Key = 0

  private def hasValidReference(register :Register) :Boolean = {
    val fields = register.fields.collect { case field :ForeignKeyField => field }
    fields.isEmpty || fields.forall { field =>
      tables.getOrElse(field.definition.target, Map[Register.Key, Register]()).contains(field.key)
    }
  }

  override def insert(register: Register): Option[Register.Key] = {
    if (hasValidReference(register)) {
      tables.get(register.definition).map { map =>
        lastKey += 1
        map.put(lastKey, register)
        lastKey
      }
    }
    else {
      None
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
    !isReferenced(registerDefinition, key) &&
      tables.get(registerDefinition).flatMap(_.remove(key)).isDefined
  }

  override def getKeysFor(registerDefinition: RegisterDefinition) = {
    tables(registerDefinition).keySet.toSet
  }

  override def replace(register: Register, key: Key): Boolean = {
    tables.get(register.definition).filter(_.contains(key)).flatMap(_.put(key, register)).isDefined
  }
}
