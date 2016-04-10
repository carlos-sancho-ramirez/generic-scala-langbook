package sword.db

/**
 * A single piece of information that can be read or written.
 * Classes inheriting this one are the possible types that the storage manager
 * is able to use. Internally they can be mapped to integers or strings
 * depending on the kind of type.
 *
 * For people used to SQL, this is mainly the representation of a type that can
 * be assigned to a column within a table. 
 */
trait FieldDefinition {

  /**
    * Creates a new field containing this value.
    * The value can be converted to match its specific type.
    * This may return None is the value is not valid or the field cannot be created.
    */
  def from(value: String, keyExtractor: String => Option[StorageManager.Key]): Option[Field]
}

/**
 * Definition for fields containing a value that must match a group identifier value within a register.
 */
trait CollectionReferenceFieldDefinition extends FieldDefinition {
  def target :CollectibleRegisterDefinition[Register]

  protected def from: Register.CollectionId => CollectionReferenceField
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]): Option[CollectionReferenceField] = {
    try {
      Some(from(value.toInt))
    }
    catch {
      case _: NumberFormatException => None
    }
  }
}

/**
 * Definition for fields containing a foreign key to a register
 */
trait ForeignKeyFieldDefinition extends FieldDefinition {
  def target :RegisterDefinition[Register]
}

object UnicodeFieldDefinition extends FieldDefinition {
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]): Option[UnicodeField] = {
    Register.unicodeTypeFrom(value).map(UnicodeField)
  }
}

object LanguageCodeFieldDefinition extends FieldDefinition {
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]): Option[LanguageCodeField] = {
    Some(LanguageCodeField(value))
  }
}

/**
 * Definition for fields containing a general-purpose char sequence (string).
 */
object CharSequenceFieldDefinition extends FieldDefinition {
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]): Option[CharSequenceField] = {
    Some(CharSequenceField(value))
  }
}

trait Field extends Equals {
  def definition :FieldDefinition
  def toString :String

  override def equals(other: Any) = {
    other != null && (other match {
      case that: Field => (that canEqual this) && definition == that.definition
      case _ => false
    })
  }
}

trait CollectionReferenceField extends Field {
  override def definition :CollectionReferenceFieldDefinition
  def collectionId :Register.CollectionId

  override def equals(other: Any) = {
    super.equals(other) && (other match {
      case that: CollectionReferenceField => collectionId == that.collectionId
      case _ => false
    })
  }

  override def hashCode = collectionId
  override def canEqual(other: Any) = other.isInstanceOf[CollectionReferenceField]
}

trait ForeignKeyField extends Field {
  override def definition :ForeignKeyFieldDefinition
  def key :StorageManager.Key

  override def equals(other: Any) = {
    super.equals(other) && (other match {
      case that: ForeignKeyField => key == that.key
      case _ => false
    })
  }

  override def hashCode = key.hashCode
  override def canEqual(other: Any) = other.isInstanceOf[ForeignKeyField]
}

case class UnicodeField(value :Register.UnicodeType) extends Field {
  override val definition = UnicodeFieldDefinition
  override val toString = value.toChar.toString

  override def equals(other: Any) = {
    super.equals(other) && (other match {
      case that: UnicodeField => value == that.value
      case _ => false
    })
  }

  override def hashCode = value
  override def canEqual(other: Any) = other.isInstanceOf[UnicodeField]
}

case class CharSequenceField(value :String) extends Field {
  override val definition = CharSequenceFieldDefinition
  override val toString = value

  override def equals(other: Any) = {
    super.equals(other) && (other match {
      case that: CharSequenceField => value == that.value
      case _ => false
    })
  }

  override def hashCode = Option(value).map(_.hashCode).getOrElse(0)
  override def canEqual(other: Any) = other.isInstanceOf[UnicodeField]
}

case class LanguageCodeField(code :Register.LanguageCode) extends Field {
  override val definition = LanguageCodeFieldDefinition
  override val toString = code

  override def equals(other: Any) = {
    super.equals(other) && (other match {
      case that: LanguageCodeField => code == that.code
      case _ => false
    })
  }

  override def hashCode = Option(code).map(_.hashCode).getOrElse(0)
  override def canEqual(other: Any) = other.isInstanceOf[UnicodeField]
}

/**
 * Sequence of fields composing a set of data.
 * In SQL terms, this is equivalent to a table row definition excluding the
 * primary key, that is always an integer.
 */
trait RegisterDefinition[+R <: Register] {
  def fields :Seq[FieldDefinition]
  def from(values: Seq[String], keyExtractor: FieldDefinition => (String => Option[StorageManager.Key])): Option[R]
}

/**
 * A register that can be grouped with its key
 */
trait CollectibleRegisterDefinition[+R <: Register] extends RegisterDefinition[R]

/**
 * A kind of register that can be grouped in different arrays
 */
trait ArrayableRegisterDefinition[+R <: Register] extends CollectibleRegisterDefinition[R]

object Register {
  type CollectionId = Int
  type Index = Int
  type Position = Int
  type UnicodeType = Int
  type LanguageCode = String // ISO 639-1: 2 lower-case char string uniquely identifying a language

  val undefinedCollection :CollectionId = 0

  def collectionIdFrom(value: String): Option[CollectionId] = {
    try {
      Some(value.toInt)
    }
    catch {
      case _: NumberFormatException => None
    }
  }

  def unicodeTypeFrom(value: String): Option[UnicodeType] = {
    try {
      Some(value.toInt)
    }
    catch {
      case _: NumberFormatException => None
    }
  }
}

trait Register {
  def definition :RegisterDefinition[Register]
  def fields :Seq[Field]

  override def equals(other: Any) = {
    other != null && (other match {
      case that: Register => definition == that.definition && fields == that.fields
      case _ => false
    })
  }
}
