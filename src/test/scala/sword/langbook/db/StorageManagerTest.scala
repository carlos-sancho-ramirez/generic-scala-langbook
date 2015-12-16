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

  it can "insert a register and retrieve it back with the given identifier" in {
    val regDefinition = new RegisterDefinition {
      override def fields: Seq[FieldDefinition] = List(ArrayIndexFieldDefinition)
    }

    val register = new Register {
      override def fields: Seq[Field] = List(ArrayIndexField(5))
      override def definition: RegisterDefinition = regDefinition
    }

    val storageManager = newStorageManager(List(regDefinition))
    val keyOption = storageManager.insert(register)
    keyOption shouldBe defined

    val regOption = storageManager.get(regDefinition, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual register
  }
}
