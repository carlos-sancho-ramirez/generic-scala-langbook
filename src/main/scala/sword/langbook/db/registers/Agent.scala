package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object Agent extends RegisterDefinition[Agent] {
  object Flags {
    // Bit 0 is reserved to decide if the correlation is to match the starting or ending of the representation
    private val fromStart = 0
    private val fromEnd = 1

    // Bit 1 determines if new words are created or the existing ones are valid
    private val nonModify = 0
    private val modify = 2

    // Bit 2 determines if the correlation is to remove or append to the given representation
    // (only relevant if bit 1 is set)
    private val remove = 0
    private val add = 4

    val matchStart = nonModify | fromStart
    val matchEnd = nonModify | fromEnd
    val removeStart = modify | remove | fromStart
    val removeEnd = modify | remove | fromEnd
    val prepend = modify | add | fromStart
    val append = modify | add | fromEnd
  }

  override def fields = Vector(
    NullableBunchReferenceFieldDefinition,
    BunchReferenceFieldDefinition,
    DiffNullableBunchReferenceFieldDefinition,
    NullableCorrelationReferenceFieldDefinition,
    IntFieldDefinition)

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        sourceBunchKey <- keyExtractor(NullableBunchReferenceFieldDefinition)(values.head)
        targetBunchKey <- keyExtractor(BunchReferenceFieldDefinition)(values(1))
        diffBunchKey <- keyExtractor(DiffNullableBunchReferenceFieldDefinition)(values.head)
        correlationId <- Register.collectionIdFrom(values(2))
        flags <- Register.intTypeFrom(values(3))
      } yield {
        Agent(sourceBunchKey, targetBunchKey, diffBunchKey, correlationId, flags)
      }
    }
    else None
  }
}

case class Agent(sourceBunch: StorageManager.Key, targetBunch: StorageManager.Key,
                 diffBunch: StorageManager.Key, correlation: Register.CollectionId,
                 flags: Register.IntType) extends Register {
  override def definition = Agent
  override def fields = Vector(
    NullableBunchReferenceField(sourceBunch),
    BunchReferenceField(targetBunch),
    DiffNullableBunchReferenceField(diffBunch),
    NullableCorrelationReferenceField(correlation),
    IntField(flags)) // TODO: Avoid using generic int field type an use proper flags field
}
