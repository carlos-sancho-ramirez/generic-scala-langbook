package sword.db

/**
 * Implementation for StorageManager that saves all its data in memory.
 * This is expected to be faster than other implementations but all data will be lost whenever this
 * class is garbage collected.
 */
class MemoryStorageManager(registerDefinitions :Seq[RegisterDefinition]) extends AbstractStorageManager(registerDefinitions) {

  private val tables :Map[RegisterDefinition, scala.collection.mutable.Map[Key, Register]] =
      registerDefinitions.map(d => (d,scala.collection.mutable.Map[Key, Register]())).toMap
  private var lastIndex :Register.Index = 0
  private var lastGroup :Register.CollectionId = 0
  private val defaultGroup = 0

  private def hasValidReference(register :Register) :Boolean = {
    val fields = register.fields.collect { case field :ForeignKeyField => field }
    fields.isEmpty || fields.forall { field =>
      tables.getOrElse(field.definition.target, Map[Key, Register]()).contains(field.key)
    }
  }

  private def insert(group: Register.CollectionId, register: Register): Option[Key] = {
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

    if (!definitions.head.isInstanceOf[CollectibleRegisterDefinition]) {
      throw new UnsupportedOperationException("Unable to insert collections for non-collectible registers")
    }

    registers.foreach( insert(group, _))
    Some(group)
  }

  override def get(key: Key): Option[Register] = {
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

  override def getKeysFor(registerDefinition: RegisterDefinition) = {
    tables(registerDefinition).keySet.toSet
  }

  override def replace(register: Register, key: Key): Boolean = {
    tables.get(register.definition).filter(_.contains(key)).flatMap(_.put(key, register)).isDefined
  }

  override def getMapFor(registerDefinition :RegisterDefinition) = {
    tables(registerDefinition)
  }

  override def getKeysForCollection(registerDefinition :CollectibleRegisterDefinition, id :Register.CollectionId) = {
    tables(registerDefinition).keys.filter(_.group == id).toSet
  }

  override def getMapForCollection(registerDefinition :CollectibleRegisterDefinition, id :Register.CollectionId) = {
    tables(registerDefinition).filterKeys(_.group == id)
  }
}
