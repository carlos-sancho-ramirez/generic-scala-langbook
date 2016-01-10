package sword.langbook.db

import org.scalatest.{Matchers, FlatSpec}

abstract class StorageManagerTest extends FlatSpec with Matchers {

  def newStorageManager(registerDefinitions: Seq[RegisterDefinition]): StorageManager

  val numFieldDef = new FieldDefinition {}

  case class numField(value :Int) extends Field {
    override val definition = numFieldDef
  }

  val numRegFieldValue = numField(23)

  val numRegDef = new CollectibleRegisterDefinition {
    override val fields = List(numFieldDef)
  }

  val numReg = new Register {
    override val fields = List(numRegFieldValue)
    override val definition = numRegDef
  }

  class NumRegister(value :Int) extends Register {
    override val fields = List(numField(value))
    override val definition = numRegDef
  }

  val numRegForeignKeyFieldDef = new ForeignKeyFieldDefinition {
    override val target = numRegDef
  }

  val numRegRefRegDef = new RegisterDefinition {
    override val fields = List(numRegForeignKeyFieldDef)
  }

  val numRegCollRefFieldDef = new CollectionReferenceFieldDefinition {
    override val target = numRegDef
  }

  val numRegCollRefRegDef = new RegisterDefinition {
    override val fields = List(numRegCollRefFieldDef)
  }

  behavior of "A storage Manager"

  it should "throw an IllegalArgumentException if a duplicated register definition is entered" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(numRegDef, numRegDef))
    }
  }

  it should "throw an IllegalArgumentException if at least one of the given register definitions has a foreign key for a register that is not included in the list" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(numRegRefRegDef))
    }
  }

  it should "throw an IllegalArgumentException if at least one of the given register definitions has a collection reference that is not included in the list" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(numRegCollRefRegDef))
    }
  }

  it can "insert a register and retrieve it back with the given identifier" in {
    val storageManager = newStorageManager(List(numRegDef))
    val keyOption = storageManager.insert(numReg)
    keyOption shouldBe defined

    val regOption = storageManager.get(numRegDef, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual numReg
  }

  it can "return a value more than once for the same key" in {
    val storageManager = newStorageManager(List(numRegDef))
    val keyOption = storageManager.insert(numReg)
    keyOption shouldBe defined

    val regOption1 = storageManager.get(numRegDef, keyOption.get)
    regOption1 shouldBe defined
    regOption1.get shouldEqual numReg

    val regOption2 = storageManager.get(numRegDef, keyOption.get)
    regOption2 shouldBe defined
    regOption2.get shouldEqual numReg
  }

  it can "insert a register and delete it using the given identifier" in {
    val storageManager = newStorageManager(List(numRegDef))
    val keyOption = storageManager.insert(numReg)
    keyOption shouldBe defined

    storageManager.delete(numRegDef, keyOption.get) shouldBe true
  }

  it can "not delete more than once for the same key" in {
    val storageManager = newStorageManager(List(numRegDef))
    val keyOption = storageManager.insert(numReg)
    keyOption shouldBe defined

    storageManager.delete(numRegDef, keyOption.get) shouldBe true
    storageManager.delete(numRegDef, keyOption.get) shouldBe false
  }

  it can "not insert a register pointing to nothing" in {
    val storageManager = newStorageManager(List(numRegDef, numRegRefRegDef))
    val reg2 = new Register {
      override val fields = List(new ForeignKeyField {
        override val key = Register.Key(0, 276)
        override val definition = numRegForeignKeyFieldDef
      })
      override val definition = numRegRefRegDef
    }

    storageManager.insert(reg2) shouldBe None
  }

  it can "not delete a register pointed by another one" in {
    val storageManager = newStorageManager(List(numRegDef, numRegRefRegDef))
    val keyOption = storageManager.insert(numReg)
    keyOption shouldBe defined

    val reg2 = new Register {
      override val fields = List(new ForeignKeyField {
        override val key = keyOption.get
        override val definition = numRegForeignKeyFieldDef
      })
      override val definition = numRegRefRegDef
    }

    val keyOption2 = storageManager.insert(reg2)
    keyOption2 shouldBe defined

    storageManager.delete(numRegDef, keyOption.get) shouldBe false
    storageManager.delete(numRegRefRegDef, keyOption2.get) shouldBe true
    storageManager.delete(numRegDef, keyOption.get) shouldBe true
  }

  it can "insert a collection in a single operation" in {
    val manager = newStorageManager(List(numRegDef))
    val reg1 = new NumRegister(5)
    val reg2 = new NumRegister(7)
    val reg3 = new NumRegister(23)

    val list = List(reg1, reg2, reg3)
    val collIdOption = manager.insert(list)
    collIdOption shouldBe defined

    val keys = manager.getKeysFor(reg1.definition)
    keys.size shouldBe list.size

    for (key <- keys) {
      key.group shouldBe collIdOption.get
    }
  }

  it should "throw an UnsupportedOperationException in case of inserting a collection for non-collectible registers" in {
    val myRegDef = new RegisterDefinition {
      override val fields = List(numFieldDef)
    }

    val manager = newStorageManager(List(myRegDef))
    class MyNumReg(value :Int) extends Register {
      override val definition = myRegDef
      override val fields = List(numField(value))
    }

    val reg1 = new MyNumReg(5)
    val reg2 = new MyNumReg(7)
    val reg3 = new MyNumReg(23)

    val list = List(reg1, reg2, reg3)
    intercept[UnsupportedOperationException] {
      manager.insert(list)
    }
  }

  it should "return a null set before inserting anything" in {
    val manager = newStorageManager(List(numRegDef))
    manager.getKeysFor(numRegDef).isEmpty shouldBe true
  }

  it should "return only the key for the register inserted" in {
    val manager = newStorageManager(List(numRegDef))
    val keyOption = manager.insert(numReg)
    keyOption shouldBe defined

    manager.getKeysFor(numRegDef).size shouldBe 1
    manager.getKeysFor(numRegDef) should contain (keyOption.get)
  }

  it should "return only the keys for registers inserted (more than one)" in {
    val manager = newStorageManager(List(numRegDef))
    val keyOption1 = manager.insert(numReg)
    keyOption1 shouldBe defined

    val keyOption2 = manager.insert(numReg)
    keyOption2 shouldBe defined

    manager.getKeysFor(numRegDef).size shouldBe 2
    manager.getKeysFor(numRegDef) should contain (keyOption1.get)
    manager.getKeysFor(numRegDef) should contain (keyOption2.get)
  }

  it can "replace one register by another with the same definition" in {
    val storageManager = newStorageManager(List(numRegDef))
    val keyOption = storageManager.insert(numReg)
    keyOption shouldBe defined

    val regB = new Register {
      override val fields = List(numField(numRegFieldValue.value + 1))
      override val definition = numRegDef
    }
    regB should not equal numReg

    storageManager.replace(regB, keyOption.get) shouldBe true

    val regOption = storageManager.get(numRegDef, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual regB
  }

  it can "not replace one register by another if the key is not defined previously" in {
    val storageManager = newStorageManager(List(numRegDef))
    val keyOption = storageManager.insert(numReg)
    keyOption shouldBe defined

    val regB = new Register {
      override val fields = List(numField(numRegFieldValue.value + 1))
      override val definition = numRegDef
    }
    regB should not equal numReg

    val newKey = Register.Key(keyOption.get.group, keyOption.get.index + 100)
    storageManager.replace(regB, newKey) shouldBe false

    val regOption = storageManager.get(numRegDef, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual numReg

    storageManager.get(numRegDef, newKey) shouldBe None
  }

  ignore should "return all register keys for those register with the given collection id" in {
    val manager = newStorageManager(List(numRegDef))
    val ids = List[Register.CollectionId](1,2,3,1,2,1)
    val keys = for (id <- ids) yield {
      manager.insert(new Register {
        override val fields = List(numField(id))
        override val definition = numRegDef
      }).get
    }

    for (id <- ids.min until ids.max) {
      val expected = ids zip keys filter { case (i,_) => i == id } map { x => x._2 }
      manager.getKeysForCollection(numRegDef, id) shouldBe expected.toSet
    }
  }

  it should "return a map containing all inserted registers and grouped by their keys" in {
    val manager = newStorageManager(List(numRegDef))
    val inserted = for (i <- 0 until 10) yield {
      val reg = new Register {
        override val fields = List(numField(i))
        override val definition = numRegDef
      }
      val result = manager.insert(reg).map(key => (key, reg))
      result shouldBe defined
      result.get
    }

    val map = manager.getMapFor(numRegDef)
    map.size shouldBe inserted.size
    for (insertion <- inserted) {
      val opt = map.get(insertion._1)
      opt shouldBe defined
      opt.get shouldBe insertion._2
    }
  }

  ignore should "return a map for all registers with the given collection id" in {
    val manager = newStorageManager(List(numRegDef))
    val ids = List[Register.CollectionId](1,2,3,1,2,1)
    val inserted = for (id <- ids) yield {
      val reg = new Register {
        override val fields = List(numField(id))
        override val definition = numRegDef
      }
      val key = manager.insert(reg).get
      (key,reg)
    }

    for (id <- ids.min until ids.max) {
      val expected = inserted.filter { case (_, reg) =>
        reg.fields.head.value == id
      }
      val map = manager.getMapForCollection(numRegDef, id)
      map.size shouldBe expected.size
      for (expectation <- expected) {
        val value = map.get(expectation._1)
        value shouldBe defined
        value.get shouldBe expectation._2
      }
    }
  }
}
