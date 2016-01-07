package sword.langbook.db

/**
 * A single piece of information that can be read or written.
 * Classes inheriting this one are the possible types that the storage manager
 * is able to use. Internally they can be mapped to integers or strings
 * depending on the kind of type.
 *
 * For people used to SQL, this is mainly the representation of a type that can
 * be assigned to a column within a table. 
 */
trait FieldDefinition

/**
 * Definition for fields containing a value that must match a group identifier value within a register.
 */
trait CollectionReferenceFieldDefinition extends FieldDefinition {
  def target :CollectibleRegisterDefinition
}

/**
 * Definition for fields containing a foreign key to a register
 */
trait ForeignKeyFieldDefinition extends FieldDefinition {
  def target :RegisterDefinition
}

object UnicodeFieldDefinition extends FieldDefinition

/**
 * Definition for fields containing a general-purpose char sequence (string).
 */
object CharSequenceFieldDefinition extends FieldDefinition

trait Field {
  def definition :FieldDefinition
  def toString :String
}

trait CollectionReferenceField extends Field {
  override def definition :CollectionReferenceFieldDefinition
  def collectionId :Register.CollectionId
}

trait ForeignKeyField extends Field {
  override def definition :ForeignKeyFieldDefinition
  def key :Register.Key
}

case class UnicodeField(value :Register.UnicodeType) extends Field {
  override val definition = UnicodeFieldDefinition
  override val toString = value.toChar.toString
}

case class CharSequenceField(value :String) extends Field {
  override val definition = CharSequenceFieldDefinition
  override val toString = value
}

/**
 * Sequence of fields composing a set of data.
 * In SQL terms, this is equivalent to a table row definition excluding the
 * primary key, that is always an integer.
 */
trait RegisterDefinition {
  def fields :Seq[FieldDefinition]
}

/**
 * A register that can be grouped with its key
 */
trait CollectibleRegisterDefinition extends RegisterDefinition

/**
 * A kind of register that can be grouped in different arrays
 */
trait ArrayableRegisterDefinition extends CollectibleRegisterDefinition

object Register {
  type CollectionId = Int
  type Index = Int
  case class Key(group :CollectionId, index :Index)
  type Position = Int
  type UnicodeType = Int

  val undefinedCollection :CollectionId = 0
}

trait Register {
  def definition :RegisterDefinition
  def fields :Seq[Field]
}
