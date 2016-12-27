package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object NullableBunchReferenceFieldDefinition extends NullableForeignKeyFieldDefinition {
  override val target = Bunch
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(NullableBunchReferenceField)
  }
}

case class NullableBunchReferenceField(override val key: StorageManager.Key) extends NullableForeignKeyField {
  override val definition = NullableBunchReferenceFieldDefinition
  override def toString = key.toString
}

/**
  * This is a copy of NullableBunchReferenceField and should be removed when possible.
  *
  * The reason of copying it is because Agent registers requires 2 references to a
  * NullableBunchReferenceField. Thus, if reusing the object instance it brings to
  * errors when trying to identify a column. E.g. to extract the column name, or
  * to filter when calling getMapFor.
  */
object DiffNullableBunchReferenceFieldDefinition extends NullableForeignKeyFieldDefinition {
  override val target = Bunch
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(DiffNullableBunchReferenceField)
  }
}

case class DiffNullableBunchReferenceField(override val key: StorageManager.Key) extends NullableForeignKeyField {
  override val definition = DiffNullableBunchReferenceFieldDefinition
  override def toString = key.toString
}

object BunchReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  override val target = Bunch
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(BunchReferenceField)
  }
}

case class BunchReferenceField(override val key: StorageManager.Key) extends ForeignKeyField {
  override val definition = BunchReferenceFieldDefinition
  override def toString = key.toString
}

object Bunch extends RegisterDefinition[Bunch] {
  override val fields = List(CharSequenceFieldDefinition)
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
  override val fields = List(CharSequenceField(name))
}