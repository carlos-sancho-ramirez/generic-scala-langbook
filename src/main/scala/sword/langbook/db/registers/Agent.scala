package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object Agent extends RegisterDefinition[Agent] {
  object Flags {
    // Bit 0 is reserved to decide if the correlation is to match the starting or ending of the representation
    val startsWith = 0
    val endsWith = 1

    // Bit 1 determines if new words are created or the existing ones are valid
    // Bit 2 determines if the correlation is to remove or append to the given representation
    val justFilter = 0
    val shouldPrependCorrelation = 4
    val shouldAppendCorrelation = 6
  }

  override def fields = Vector(
    NullableBunchReferenceFieldDefinition,
    BunchReferenceFieldDefinition,
    CorrelationReferenceFieldDefinition,
    IntFieldDefinition)

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        sourceBunchKey <- keyExtractor(NullableBunchReferenceFieldDefinition)(values.head)
        targetBunchKey <- keyExtractor(BunchReferenceFieldDefinition)(values(1))
        correlationId <- Register.collectionIdFrom(values(2))
        flags <- Register.intTypeFrom(values(3))
      } yield {
        Agent(sourceBunchKey, targetBunchKey, correlationId, flags)
      }
    }
    else None
  }
}

case class Agent(sourceBunch: StorageManager.Key, targetBunch: StorageManager.Key,
                 correlation: Register.CollectionId, flags: Register.IntType) extends Register {
  override def definition = Agent
  override def fields = Vector(
    NullableBunchReferenceField(sourceBunch),
    BunchReferenceField(targetBunch),
    CorrelationReferenceField(correlation),
    IntField(flags)) // TODO: Avoid using generic int field type an use proper flags field
}
