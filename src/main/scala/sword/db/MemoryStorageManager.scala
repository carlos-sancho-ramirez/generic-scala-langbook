package sword.db

/**
 * Implementation for StorageManager that saves all its data in memory.
 * This is expected to be faster than other implementations but all data will be lost whenever this
 * class is garbage collected.
 */
class MemoryStorageManager(registerDefinitions :Seq[RegisterDefinition[Register]]) extends AbstractStorageManager(registerDefinitions) {

  private val tables :Map[RegisterDefinition[Register], scala.collection.mutable.Map[Key, Register]] =
      registerDefinitions.map(d => (d,scala.collection.mutable.Map[Key, Register]())).toMap
  private var lastIndex :Register.Index = 0
  private var lastGroup :Register.CollectionId = 0
  private val defaultGroup = 0

  private def throwIfWrongRegister(register :Register): Unit = {
    val regDef = register.definition
    if (register.fields.size != regDef.fields.size) {
      throw new IllegalArgumentException("Invalid register: Number of fields in the register and " +
        "its definition do not match")
    }

    for { f <- register.fields zip regDef.fields} {
      if (f._1.definition != f._2) {
        throw new IllegalArgumentException("invalid register: Fields do not match its definitions")
      }
    }
  }

  private def hasValidReference(register :Register) :Boolean = {
    val fields = register.fields.collect { case field :ForeignKeyField => field }
    fields.isEmpty || fields.forall { field =>
      tables.getOrElse(field.definition.target, Map[Key, Register]()).contains(field.key)
    }
  }

  override def insert(group: Register.CollectionId, register: Register): Option[Key] = {
    throwIfWrongRegister(register)

    if (hasValidReference(register)) {
      val regDef = register.definition
      tables.get(regDef).map { map =>
        lastIndex += 1
        val key = obtainKey(regDef, group, lastIndex)
        map.put(key, register)
        key
      }
    }
    else {
      None
    }
  }

  override def insert(register: Register) = insert(defaultGroup, register)

  override def insert(registers :Traversable[Register]) = {
    lastGroup += 1
    val group = lastGroup
    val definitions = registers.map(_.definition).toSet

    if (definitions.size != 1) {
      throw new UnsupportedOperationException("Unable to insert collections for registers with different definitions")
    }

    if (!definitions.head.isInstanceOf[CollectibleRegisterDefinition[Register]]) {
      throw new UnsupportedOperationException("Unable to insert collections for non-collectible registers")
    }

    registers.foreach( insert(group, _))
    Some(group)
  }

  override def get(key: Key): Option[Register] = {
    if (key.storageManager != this) {
      throw new IllegalArgumentException("This key do not belong to this storage manager")
    }

    tables.get(key.registerDefinition).flatMap(_.get(key))
  }

  private def isReferenced(key: Key): Boolean = {
    val referencers = reverseReferences.getOrElse(key.registerDefinition, Nil)
    if (referencers.nonEmpty) {
      val references = for {
        sourceRegDef <- referencers
        table <- tables.get(sourceRegDef)
      } yield {
        table.exists { case (_,r) =>
          r.fields.collect { case field :ForeignKeyField => field }
            .exists(field => field.definition.target == key.registerDefinition && field.key == key)
        }
      }

      references.exists(x => x)
    }
    else false
  }

  override def delete(key: Key): Boolean = {
    !isReferenced(key) && tables.get(key.registerDefinition).flatMap(_.remove(key)).isDefined
  }

  override def getKeysFor(registerDefinition: RegisterDefinition[Register]) = {
    tables(registerDefinition).keySet.toSet
  }

  override def replace(register: Register, key: Key): Boolean = {
    tables.get(register.definition).filter(_.contains(key)).flatMap(_.put(key, register)).isDefined
  }

  override def getMapFor[R <: Register](registerDefinition :RegisterDefinition[R]) = {
    tables(registerDefinition).asInstanceOf[scala.collection.Map[StorageManager.Key, R]]
  }

  override def getKeysForCollection(registerDefinition :CollectibleRegisterDefinition[Register], id :Register.CollectionId) = {
    tables(registerDefinition).keys.filter(_.group == id).toSet
  }

  override def getMapForCollection[R <: Register](registerDefinition :CollectibleRegisterDefinition[R], id :Register.CollectionId) = {
    tables(registerDefinition).filterKeys(_.group == id).asInstanceOf[scala.collection.Map[Key, R]]
  }
}
