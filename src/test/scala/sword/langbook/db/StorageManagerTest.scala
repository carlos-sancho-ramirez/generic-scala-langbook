package sword.langbook.db

import org.scalatest.{Matchers, FlatSpec}

abstract class StorageManagerTest extends FlatSpec with Matchers {

  def newStorageManager(registerDefinitions: Seq[RegisterDefinition]): StorageManager

  val regDefinition = new RegisterDefinition {
    override val fields = List(ArrayIndexFieldDefinition)
  }

  val reg = new Register {
    override val fields = List(ArrayIndexField(5))
    override val definition = regDefinition
  }

  val reg1ForeignKey = new ForeignKeyFieldDefinition {
    override val target = regDefinition
  }

  val regDefinition2 = new RegisterDefinition {
    override val fields = List(reg1ForeignKey)
  }

  behavior of "A storage Manager"

  it should "throw an IllegalArgumentException if a duplicated register definition is entered" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(regDefinition, regDefinition))
    }
  }

  it should "throw an IllegalArgumentException if at least one of the given register definitions has a foreign key for a register that is not included in the list" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(regDefinition2))
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

  it can "not delete a register pointed by another one" in {
    val storageManager = newStorageManager(List(regDefinition, regDefinition2))
    val keyOption = storageManager.insert(reg)
    keyOption shouldBe defined

    val reg2 = new Register {
      override val fields = List(new ForeignKeyField {
        override val key = keyOption.get
        override val definition = reg1ForeignKey
      })
      override val definition = regDefinition2
    }

    val keyOption2 = storageManager.insert(reg2)
    keyOption2 shouldBe defined

    storageManager.delete(regDefinition, keyOption.get) shouldBe false
    storageManager.delete(regDefinition2, keyOption2.get) shouldBe true
    storageManager.delete(regDefinition, keyOption.get) shouldBe true
  }
}
