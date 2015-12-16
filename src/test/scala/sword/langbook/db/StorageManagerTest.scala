package sword.langbook.db

import org.scalatest.{Matchers, FlatSpec}

abstract class StorageManagerTest extends FlatSpec with Matchers {

  def newStorageManager(registerDefinitions: Seq[RegisterDefinition]): StorageManager

  val regDefinition = new RegisterDefinition {
    override def fields: Seq[FieldDefinition] = List(ArrayIndexFieldDefinition)
  }

  val reg = new Register {
    override def fields: Seq[Field] = List(ArrayIndexField(5))
    override def definition: RegisterDefinition = regDefinition
  }


  behavior of "A storage Manager"

  it should "throw an IllegalArgumentException if a duplicated register definition is entered" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(regDefinition, regDefinition))
    }
  }

  it can "insert a register and retrieve it back with the given identifier" in {
    val storageManager = newStorageManager(List(regDefinition))
    val keyOption = storageManager.insert(reg)
    keyOption shouldBe defined

    val regOption = storageManager.get(regDefinition, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual reg
  }

  it can "return a value more than once for the same key" in {
    val storageManager = newStorageManager(List(regDefinition))
    val keyOption = storageManager.insert(reg)
    keyOption shouldBe defined

    val regOption1 = storageManager.get(regDefinition, keyOption.get)
    regOption1 shouldBe defined
    regOption1.get shouldEqual reg

    val regOption2 = storageManager.get(regDefinition, keyOption.get)
    regOption2 shouldBe defined
    regOption2.get shouldEqual reg
  }

  it can "insert a register and delete it using the given identifier" in {
    val storageManager = newStorageManager(List(regDefinition))
    val keyOption = storageManager.insert(reg)
    keyOption shouldBe defined

    storageManager.delete(regDefinition, keyOption.get) shouldBe true
  }

  it can "not delete more than once for the same key" in {
    val storageManager = newStorageManager(List(regDefinition))
    val keyOption = storageManager.insert(reg)
    keyOption shouldBe defined

    storageManager.delete(regDefinition, keyOption.get) shouldBe true
    storageManager.delete(regDefinition, keyOption.get) shouldBe false
  }
}
