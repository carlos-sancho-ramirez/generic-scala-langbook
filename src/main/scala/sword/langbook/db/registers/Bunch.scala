package sword.langbook.db.registers

import sword.db._
import sword.db.StorageManager.Key

object Bunch extends RegisterDefinition[Bunch] {
  object CharSequenceField extends CharSequenceFieldDefinition {
    override def newField = apply
  }
  case class CharSequenceField(override val value: String) extends AbstractCharSequenceField {
    override val definition = CharSequenceField
  }

  override val fields = List(CharSequenceField)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => (String) => Option[Key]) = {
    if (values.size == fields.size) {
      Some(Bunch(values.head))
    }
    else None
  }
}

case class Bunch(name: String) extends Register {
  override val definition = Bunch
  override val fields = List(Bunch.CharSequenceField(name))
}

trait BunchReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def newField: Key => AbstractBunchReferenceField
  override val target = Bunch
  override def from(value: String, keyExtractor: String => Option[Key]) = {
    keyExtractor(value).map(newField)
  }
}

trait AbstractBunchReferenceField extends ForeignKeyField {
  override def definition: BunchReferenceFieldDefinition
}

trait NullableBunchReferenceFieldDefinition extends NullableForeignKeyFieldDefinition {
  def newField: Key => AbstractNullableBunchReferenceField
  override val target = Bunch
  override def from(value: String, keyExtractor: String => Option[Key]) = {
    keyExtractor(value).map(newField)
  }
}

trait AbstractNullableBunchReferenceField extends NullableForeignKeyField {
  override def definition: NullableBunchReferenceFieldDefinition
}
