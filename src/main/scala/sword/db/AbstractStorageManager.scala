package sword.db

/**
 * Abstract class including common implementation that it is expected on each StorageManager implementation.
 */
abstract class AbstractStorageManager(override val registerDefinitions :Seq[RegisterDefinition[Register]]) extends StorageManager {

  if (registerDefinitions.toSet.size < registerDefinitions.size) {
    throw new IllegalArgumentException("Duplicated register definitions are not allowed")
  }

  /**
   * List all connections between register definitions, being the first the source and the second
   * the target definition.
   */
  val singleReferences :Seq[(RegisterDefinition[Register], RegisterDefinition[Register])] = for {
    regDef <- registerDefinitions
    fieldDef <- regDef.fields if fieldDef.isInstanceOf[ForeignKeyFieldDefinition]
  } yield {
    (regDef, fieldDef.asInstanceOf[ForeignKeyFieldDefinition].target)
  }

  if (singleReferences.exists { case (_,target) => !registerDefinitions.contains(target) }) {
    throw new IllegalArgumentException("All given register definitions that include a foreign key" +
        " field must have as target one of the definitions given")
  }

  val groupReferences :Seq[(RegisterDefinition[Register], CollectibleRegisterDefinition[Register])] = for {
    regDef <- registerDefinitions
    fieldDef <- regDef.fields if fieldDef.isInstanceOf[CollectionReferenceFieldDefinition]
  } yield {
    (regDef, fieldDef.asInstanceOf[CollectionReferenceFieldDefinition].target)
  }

  if (groupReferences.exists { case (_,target) => !registerDefinitions.contains(target) }) {
    throw new IllegalArgumentException("All given register definitions that include a collection" +
        " reference field must have as target one of the definitions given")
  }

  val reverseReferences :Map[RegisterDefinition[Register], Seq[RegisterDefinition[Register]]] =
      singleReferences.groupBy{ case (s,t) => t}.map { case (t, seq) => (t, seq.map(_._1))}
}
