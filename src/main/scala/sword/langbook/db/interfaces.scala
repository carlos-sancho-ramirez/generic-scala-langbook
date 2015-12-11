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
 * Definition for fields containing a general-purpose char sequence (string).
 */
object CharSequenceFieldDefinition extends FieldDefinition

/**
 * Definition for fields containing a foreign key to register
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
object ArrayIndexFieldDefinition extends FieldDefinition

trait Field {
  def definition :FieldDefinition
  def toString :String
}

case class CharSequenceField(value :String) extends Field {
  override val definition = CharSequenceFieldDefinition
  override val toString = value
}

case class UnicodeField(value :Register.UnicodeType) extends Field {
  override val definition = UnicodeFieldDefinition
  override val toString = value.toChar.toString
}

case class ArrayIndexField(index :Register.Index) extends Field {
  override val definition = ArrayIndexFieldDefinition
  override val toString = index.toString
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
  type Index = Int
  type Key = Long
  type Position = Int
  type UnicodeType = Int
}

trait Register {
  def definition :RegisterDefinition
  def fields :Seq[Field]
}
