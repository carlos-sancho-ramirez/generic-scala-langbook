package sword.langbook.db

import sword.langbook.db.Register.Key

/**
 * Implementation for StorageManager that saves all its data in memory.
 * This is expected to be faster than other implementations but all data will be lost whenever this
 * class is garbage collected.
 */
class MemoryStorageManager(registerDefinitions :Seq[RegisterDefinition]) extends AbstractStorageManager(registerDefinitions) {

  private val tables :Map[RegisterDefinition, scala.collection.mutable.Map[Register.Index, Register]] =
      registerDefinitions.map(d => (d,scala.collection.mutable.Map[Register.Index, Register]())).toMap
  private var lastIndex :Register.Index = 0
  private val defaultGroup = 0

  private def hasValidReference(register :Register) :Boolean = {
    val fields = register.fields.collect { case field :ForeignKeyField => field }
    fields.isEmpty || fields.forall { field =>
      tables.getOrElse(field.definition.target, Map[Register.Index, Register]()).contains(field.key.index)
    }
  }

  override def insert(register: Register): Option[Register.Key] = {
    if (hasValidReference(register)) {
      tables.get(register.definition).map { map =>
        lastIndex += 1
        map.put(lastIndex, register)
        Key(defaultGroup, lastIndex)
      }
    }
    else {
      None
    }
  }

  override def get(registerDefinition: RegisterDefinition, key: Register.Key): Option[Register] = {
    tables.get(registerDefinition).flatMap(_.get(key.index))
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
      tables.get(registerDefinition).flatMap(_.remove(key.index)).isDefined
  }

  override def getKeysFor(registerDefinition: RegisterDefinition) = {
    tables(registerDefinition).keySet.map(Key(defaultGroup,_)).toSet
  }

  override def replace(register: Register, key: Key): Boolean = {
    val index = key.index
    tables.get(register.definition).filter(_.contains(index)).flatMap(_.put(index, register)).isDefined
  }

  override def getMapFor(registerDefinition :RegisterDefinition) = {
    tables(registerDefinition).map{ case (k,v) => (Key(defaultGroup, k), v) }
  }
}
