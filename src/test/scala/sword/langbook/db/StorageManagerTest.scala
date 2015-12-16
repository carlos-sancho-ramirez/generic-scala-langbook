package sword.langbook.db

import org.scalatest.{Matchers, FlatSpec}

abstract class StorageManagerTest extends FlatSpec with Matchers {

  def newStorageManager(registerDefinitions: Seq[RegisterDefinition]): StorageManager

  "A Storage Manager" should "throw an IllegalArgumentException if a duplicated register definition is entered" in {
    val regDefinition = new RegisterDefinition {
      override def fields: Seq[FieldDefinition] = List(ArrayIndexFieldDefinition)
    }

    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(regDefinition, regDefinition))
    }
  }
}
