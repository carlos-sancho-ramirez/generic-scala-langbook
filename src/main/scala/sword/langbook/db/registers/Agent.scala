package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._
import sword.langbook.db.redundant.Text.SymbolArrayReferenceField
import sword.langbook.db.registers.Language.ConceptReferenceField

object Agent extends RegisterDefinition[Agent] {
  object Flags {
    // Bit 0 is reserved to decide if the correlation is to match the starting or ending of the representation
    private val fromStart = 0
    private val fromEnd = 1

    // Bit 1 determines if new words are created or the existing ones are valid
    private val nonModify = 0
    private val modify = 2

    // Bit 2 determines if the correlation is to remove or append to the given representation
    // (only relevant if bit 1 is set, if bit 1 is cleared this should be cleared as well)
    private val remove = 0
    private val add = 4

    val matchStart = nonModify | fromStart
    val matchEnd = nonModify | fromEnd
    val removeStart = modify | remove | fromStart
    val removeEnd = modify | remove | fromEnd
    val prepend = modify | add | fromStart
    val append = modify | add | fromEnd

    def startSide(flags: Int) = (flags & fromEnd) == 0
    def shouldFilterFromSource(flags: Int) = (flags & add) == 0
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

  object CorrelationReferenceField extends NullableCorrelationReferenceFieldDefinition {
    override def newField = apply
  }
  case class CorrelationReferenceField(override val collectionId: Register.CollectionId) extends AbstractNullableCorrelationReferenceField {
    override val definition = CorrelationReferenceField
  }

  override def fields = Vector(
    SourceBunchReferenceField,
    TargetBunchReferenceField,
    DiffBunchReferenceField,
    CorrelationReferenceField,
    IntFieldDefinition)

  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        sourceBunchKey <- keyExtractor(SourceBunchReferenceField)(values.head)
        targetBunchKey <- keyExtractor(TargetBunchReferenceField)(values(1))
        diffBunchKey <- keyExtractor(DiffBunchReferenceField)(values(2))
        correlationId <- Register.collectionIdFrom(values(3))
        flags <- Register.intTypeFrom(values(4))
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
    Agent.SourceBunchReferenceField(sourceBunch),
    Agent.TargetBunchReferenceField(targetBunch),
    Agent.DiffBunchReferenceField(diffBunch),
    Agent.CorrelationReferenceField(correlation),
    IntField(flags)) // TODO: Avoid using generic int field type an use proper flags field
}
