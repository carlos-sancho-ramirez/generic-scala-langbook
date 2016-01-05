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
 * When collections are defined within table, following a relational database structure, this identifier
 * is here to group different registers together. All registers with the same identifier
 * are understood to be part of the same collection.
 *
 * A register may have more than one collection identifier field in case register may be grouped in
 * different forms. It is important then that every identifier field definition has its own
 * instance in order to distinguish them. That's why this is a trait and not an object.
 */
trait CollectionIdentifierFieldDefinition extends FieldDefinition

/**
 * Definition for fields containing a value that must match a set identifier value within a register.
 */
trait CollectionReferenceFieldDefinition extends FieldDefinition {
  def target :CollectionIdentifierFieldDefinition
}

/**
 * Definition for fields containing a foreign key to a register
 */
trait ForeignKeyFieldDefinition extends FieldDefinition {
  def target :RegisterDefinition
}

object UnicodeFieldDefinition extends FieldDefinition

/**
 * Field that contains 0 or a natural number. Never will be null.
 *
 * The value contained in this field is used as an index for a list or array,
 * understanding the 0 as the first position, 1 for the second and so on.
 */
trait ArrayIndexFieldDefinition extends FieldDefinition {
  def collection :CollectionIdentifierFieldDefinition
}

/**
 * Definition for fields containing a general-purpose char sequence (string).
 */
object CharSequenceFieldDefinition extends FieldDefinition

trait Field {
  def definition :FieldDefinition
  def toString :String
}

trait CollectionIdentifierField extends Field {
  override def definition :CollectionIdentifierFieldDefinition
  def value :Register.CollectionId
  override val toString = value.toString
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

trait ArrayIndexField extends Field {
  def index :Register.Index
  override def definition :ArrayIndexFieldDefinition
  override val toString = index.toString
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
