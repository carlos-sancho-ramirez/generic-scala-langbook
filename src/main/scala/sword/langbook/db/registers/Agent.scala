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

    val matchStart = nonModify | fromStart
    val matchEnd = nonModify | fromEnd
    val modifyStart = modify | fromStart
    val modifyEnd = modify | fromEnd

    def startSide(flags: Int) = (flags & fromEnd) == 0
    def shouldModify(flags: Int) = (flags & modify) != 0
  }

  object SourceBunchReferenceField extends NullableBunchReferenceFieldDefinition {
    override def newField = apply
  }
  case class SourceBunchReferenceField(override val key: Key) extends AbstractNullableBunchReferenceField {
    override val definition = SourceBunchReferenceField
  }

  object TargetBunchReferenceField extends BunchReferenceFieldDefinition {
    override def newField = apply
  }
  case class TargetBunchReferenceField(override val key: Key) extends AbstractBunchReferenceField {
    override val definition = TargetBunchReferenceField
  }

  object DiffBunchReferenceField extends NullableBunchReferenceFieldDefinition {
    override def newField = apply
  }
  case class DiffBunchReferenceField(override val key: Key) extends AbstractNullableBunchReferenceField {
    override val definition = DiffBunchReferenceField
  }

  object MatchCorrelationReferenceField extends NullableCorrelationReferenceFieldDefinition {
    override def newField = apply
  }
  case class MatchCorrelationReferenceField(override val collectionId: Register.CollectionId) extends AbstractNullableCorrelationReferenceField {
    override val definition = MatchCorrelationReferenceField
  }

  object AddCorrelationReferenceField extends NullableCorrelationReferenceFieldDefinition {
    override def newField = apply
  }
  case class AddCorrelationReferenceField(override val collectionId: Register.CollectionId) extends AbstractNullableCorrelationReferenceField {
    override val definition = AddCorrelationReferenceField
  }

  override def fields = Vector(
    SourceBunchReferenceField,
    TargetBunchReferenceField,
    DiffBunchReferenceField,
    MatchCorrelationReferenceField,
    AddCorrelationReferenceField,
    IntFieldDefinition)

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        sourceBunchKey <- keyExtractor(SourceBunchReferenceField)(values.head)
        targetBunchKey <- keyExtractor(TargetBunchReferenceField)(values(1))
        diffBunchKey <- keyExtractor(DiffBunchReferenceField)(values(2))
        matchCorrelationId <- Register.collectionIdFrom(values(3))
        addCorrelationId <- Register.collectionIdFrom(values(4))
        flags <- Register.intTypeFrom(values(5))
      } yield {
        Agent(sourceBunchKey, targetBunchKey, diffBunchKey, matchCorrelationId, addCorrelationId, flags)
      }
    }
    else None
  }
}

case class Agent(sourceBunch: StorageManager.Key, targetBunch: StorageManager.Key,
                 diffBunch: StorageManager.Key, matchCorrelation: Register.CollectionId,
                 addCorrelation: Register.CollectionId, flags: Register.IntType) extends Register {
  override def definition = Agent
  override def fields = Vector(
    Agent.SourceBunchReferenceField(sourceBunch),
    Agent.TargetBunchReferenceField(targetBunch),
    Agent.DiffBunchReferenceField(diffBunch),
    Agent.MatchCorrelationReferenceField(matchCorrelation),
    Agent.AddCorrelationReferenceField(addCorrelation),
    IntField(flags)) // TODO: Avoid using generic int field type an use proper flags field
}
