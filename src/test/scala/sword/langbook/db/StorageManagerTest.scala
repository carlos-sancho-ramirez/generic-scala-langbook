package sword.langbook.db

import org.scalatest.{Matchers, FlatSpec}
import sword.langbook.db.Register.SetId

abstract class StorageManagerTest extends FlatSpec with Matchers {

  def newStorageManager(registerDefinitions: Seq[RegisterDefinition]): StorageManager

  object Reg1SetIdentifierFieldDefinition extends SetIdentifierFieldDefinition

  val regDefinition = new RegisterDefinition {
    override val fields = List(Reg1SetIdentifierFieldDefinition)
  }

  val regSetId :Register.SetId = 23

  val reg = new Register {
    override val fields = List(SetIdentifierField(Reg1SetIdentifierFieldDefinition, regSetId))
    override val definition = regDefinition
  }

  val reg1ForeignKey = new ForeignKeyFieldDefinition {
    override val target = regDefinition
  }

  val regDefinition2 = new RegisterDefinition {
    override val fields = List(reg1ForeignKey)
  }

  val reg3SetRefFieldDef = new SetReferenceFieldDefinition {
    override val target = Reg1SetIdentifierFieldDefinition
  }

  val regDefinition3 = new RegisterDefinition {
    override val fields = List(reg3SetRefFieldDef)
  }

  behavior of "A storage Manager"

  it should "throw an IllegalArgumentException if a duplicated register definition is entered" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(regDefinition, regDefinition))
    }
  }

  it should "throw an IllegalArgumentException if a duplicated set definition is entered" in {
    an [IllegalArgumentException] should be thrownBy {
      val regDef = new RegisterDefinition {
        override val fields = Vector(Reg1SetIdentifierFieldDefinition, ArrayIndexFieldDefinition)
      }

      newStorageManager(List(regDefinition, regDef))
    }
  }

  it should "throw an IllegalArgumentException if at least one of the given register definitions has a foreign key for a register that is not included in the list" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(regDefinition2))
    }
  }

  it should "throw an IllegalArgumentException if at least one of the given register definitions has a set reference that is not included in the list" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(regDefinition3))
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

  it can "not insert a register pointing to nothing" in {
    val storageManager = newStorageManager(List(regDefinition, regDefinition2))
    val reg2 = new Register {
      override val fields = List(new ForeignKeyField {
        override val key = 276L
        override val definition = reg1ForeignKey
      })
      override val definition = regDefinition2
    }

    storageManager.insert(reg2) shouldBe None
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

  it should "return a null set before inserting anything" in {
    val manager = newStorageManager(List(regDefinition))
    manager.getKeysFor(regDefinition).isEmpty shouldBe true
  }

  it should "return only the key for the register inserted" in {
    val manager = newStorageManager(List(regDefinition))
    val keyOption = manager.insert(reg)
    keyOption shouldBe defined

    manager.getKeysFor(regDefinition).size shouldBe 1
    manager.getKeysFor(regDefinition) should contain (keyOption.get)
  }

  it should "return only the keys for registers inserted (more than one)" in {
    val manager = newStorageManager(List(regDefinition))
    val keyOption1 = manager.insert(reg)
    keyOption1 shouldBe defined

    val keyOption2 = manager.insert(reg)
    keyOption2 shouldBe defined

    manager.getKeysFor(regDefinition).size shouldBe 2
    manager.getKeysFor(regDefinition) should contain (keyOption1.get)
    manager.getKeysFor(regDefinition) should contain (keyOption2.get)
  }

  it can "replace one register by another with the same definition" in {
    val storageManager = newStorageManager(List(regDefinition))
    val keyOption = storageManager.insert(reg)
    keyOption shouldBe defined

    val regB = new Register {
      override val fields = List(SetIdentifierField(Reg1SetIdentifierFieldDefinition, regSetId + 1))
      override val definition = regDefinition
    }
    regB should not equal reg

    storageManager.replace(regB, keyOption.get) shouldBe true

    val regOption = storageManager.get(regDefinition, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual regB
  }

  it can "not replace one register by another if the key is not defined previously" in {
    val storageManager = newStorageManager(List(regDefinition))
    val keyOption = storageManager.insert(reg)
    keyOption shouldBe defined

    val regB = new Register {
      override val fields = List(SetIdentifierField(Reg1SetIdentifierFieldDefinition, regSetId + 1))
      override val definition = regDefinition
    }
    regB should not equal reg

    val newKey = keyOption.get + 100
    storageManager.replace(regB, newKey) shouldBe false

    val regOption = storageManager.get(regDefinition, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual reg

    storageManager.get(regDefinition, newKey) shouldBe None
  }
}
