package sword.langbook.db

import org.scalatest.{Matchers, FlatSpec}

abstract class StorageManagerTest extends FlatSpec with Matchers {

  def newStorageManager(registerDefinitions: Seq[RegisterDefinition]): StorageManager

  object setFieldDef extends SetIdentifierFieldDefinition

  val regDefWithSet = new RegisterDefinition {
    override val fields = List(setFieldDef)
  }

  val regSetId :Register.SetId = 23

  val regWithSet = new Register {
    override val fields = List(SetIdentifierField(setFieldDef, regSetId))
    override val definition = regDefWithSet
  }

  val reg1ForeignKey = new ForeignKeyFieldDefinition {
    override val target = regDefWithSet
  }

  val regDefinition2 = new RegisterDefinition {
    override val fields = List(reg1ForeignKey)
  }

  val reg3SetRefFieldDef = new SetReferenceFieldDefinition {
    override val target = setFieldDef
  }

  val regDefinition3 = new RegisterDefinition {
    override val fields = List(reg3SetRefFieldDef)
  }

  behavior of "A storage Manager"

  it should "throw an IllegalArgumentException if a duplicated register definition is entered" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(regDefWithSet, regDefWithSet))
    }
  }

  it should "throw an IllegalArgumentException if a duplicated set definition is entered" in {
    an [IllegalArgumentException] should be thrownBy {
      val regDef = new RegisterDefinition {
        override val fields = Vector(setFieldDef, ArrayIndexFieldDefinition)
      }

      newStorageManager(List(regDefWithSet, regDef))
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
    val storageManager = newStorageManager(List(regDefWithSet))
    val keyOption = storageManager.insert(regWithSet)
    keyOption shouldBe defined

    val regOption = storageManager.get(regDefWithSet, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual regWithSet
  }

  it can "return a value more than once for the same key" in {
    val storageManager = newStorageManager(List(regDefWithSet))
    val keyOption = storageManager.insert(regWithSet)
    keyOption shouldBe defined

    val regOption1 = storageManager.get(regDefWithSet, keyOption.get)
    regOption1 shouldBe defined
    regOption1.get shouldEqual regWithSet

    val regOption2 = storageManager.get(regDefWithSet, keyOption.get)
    regOption2 shouldBe defined
    regOption2.get shouldEqual regWithSet
  }

  it can "insert a register and delete it using the given identifier" in {
    val storageManager = newStorageManager(List(regDefWithSet))
    val keyOption = storageManager.insert(regWithSet)
    keyOption shouldBe defined

    storageManager.delete(regDefWithSet, keyOption.get) shouldBe true
  }

  it can "not delete more than once for the same key" in {
    val storageManager = newStorageManager(List(regDefWithSet))
    val keyOption = storageManager.insert(regWithSet)
    keyOption shouldBe defined

    storageManager.delete(regDefWithSet, keyOption.get) shouldBe true
    storageManager.delete(regDefWithSet, keyOption.get) shouldBe false
  }

  it can "not insert a register pointing to nothing" in {
    val storageManager = newStorageManager(List(regDefWithSet, regDefinition2))
    val reg2 = new Register {
      override val fields = List(new ForeignKeyField {
        override val key = 276
        override val definition = reg1ForeignKey
      })
      override val definition = regDefinition2
    }

    storageManager.insert(reg2) shouldBe None
  }

  it can "not delete a register pointed by another one" in {
    val storageManager = newStorageManager(List(regDefWithSet, regDefinition2))
    val keyOption = storageManager.insert(regWithSet)
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

    storageManager.delete(regDefWithSet, keyOption.get) shouldBe false
    storageManager.delete(regDefinition2, keyOption2.get) shouldBe true
    storageManager.delete(regDefWithSet, keyOption.get) shouldBe true
  }

  it should "return a null set before inserting anything" in {
    val manager = newStorageManager(List(regDefWithSet))
    manager.getKeysFor(regDefWithSet).isEmpty shouldBe true
  }

  it should "return only the key for the register inserted" in {
    val manager = newStorageManager(List(regDefWithSet))
    val keyOption = manager.insert(regWithSet)
    keyOption shouldBe defined

    manager.getKeysFor(regDefWithSet).size shouldBe 1
    manager.getKeysFor(regDefWithSet) should contain (keyOption.get)
  }

  it should "return only the keys for registers inserted (more than one)" in {
    val manager = newStorageManager(List(regDefWithSet))
    val keyOption1 = manager.insert(regWithSet)
    keyOption1 shouldBe defined

    val keyOption2 = manager.insert(regWithSet)
    keyOption2 shouldBe defined

    manager.getKeysFor(regDefWithSet).size shouldBe 2
    manager.getKeysFor(regDefWithSet) should contain (keyOption1.get)
    manager.getKeysFor(regDefWithSet) should contain (keyOption2.get)
  }

  it can "replace one register by another with the same definition" in {
    val storageManager = newStorageManager(List(regDefWithSet))
    val keyOption = storageManager.insert(regWithSet)
    keyOption shouldBe defined

    val regB = new Register {
      override val fields = List(SetIdentifierField(setFieldDef, regSetId + 1))
      override val definition = regDefWithSet
    }
    regB should not equal regWithSet

    storageManager.replace(regB, keyOption.get) shouldBe true

    val regOption = storageManager.get(regDefWithSet, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual regB
  }

  it can "not replace one register by another if the key is not defined previously" in {
    val storageManager = newStorageManager(List(regDefWithSet))
    val keyOption = storageManager.insert(regWithSet)
    keyOption shouldBe defined

    val regB = new Register {
      override val fields = List(SetIdentifierField(setFieldDef, regSetId + 1))
      override val definition = regDefWithSet
    }
    regB should not equal regWithSet

    val newKey = keyOption.get + 100
    storageManager.replace(regB, newKey) shouldBe false

    val regOption = storageManager.get(regDefWithSet, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual regWithSet

    storageManager.get(regDefWithSet, newKey) shouldBe None
  }

  it should "return all register keys for those register with the given set id" in {
    val manager = newStorageManager(List(regDefWithSet))
    val ids = List[Register.SetId](1,2,3,1,2,1)
    val keys = for (id <- ids) yield {
      manager.insert(new Register {
        override val fields: Seq[Field] = List(SetIdentifierField(setFieldDef, id))
        override val definition = regDefWithSet
      }).get
    }

    for (id <- ids.min until ids.max) {
      val expected = ids zip keys filter { case (i,_) => i == id } map { x => x._2 }
      manager.getKeysForSet(setFieldDef, id) shouldBe expected.toSet
    }
  }

  it should "return a map containing all inserted registers and grouped by their keys" in {
    val manager = newStorageManager(List(regDefWithSet))
    val inserted = for (i <- 0 until 10) yield {
      val reg = new Register {
        override val fields = List(SetIdentifierField(setFieldDef, i))
        override val definition: RegisterDefinition = regDefWithSet
      }
      val result = manager.insert(reg).map(key => (key, reg))
      result shouldBe defined
      result.get
    }

    val map = manager.getMapFor(regDefWithSet)
    map.size shouldBe inserted.size
    for (insertion <- inserted) {
      val opt = map.get(insertion._1)
      opt shouldBe defined
      opt.get shouldBe insertion._2
    }
  }

  it should "return a map for all registers with the given set id" in {
    val manager = newStorageManager(List(regDefWithSet))
    val ids = List[Register.SetId](1,2,3,1,2,1)
    val inserted = for (id <- ids) yield {
      val reg = new Register {
        override val fields = List(SetIdentifierField(setFieldDef, id))
        override val definition = regDefWithSet
      }
      val key = manager.insert(reg).get
      (key,reg)
    }

    for (id <- ids.min until ids.max) {
      val expected = inserted.filter { case (_, reg) =>
        reg.fields.head.value == id
      }
      val map = manager.getMapForSet(setFieldDef, id)
      map.size shouldBe expected.size
      for (expectation <- expected) {
        val value = map.get(expectation._1)
        value shouldBe defined
        value.get shouldBe expectation._2
      }
    }
  }
}
