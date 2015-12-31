package sword.langbook.db

import org.scalatest.{Matchers, FlatSpec}

abstract class StorageManagerTest extends FlatSpec with Matchers {

  def newStorageManager(registerDefinitions: Seq[RegisterDefinition]): StorageManager

  object collectionFieldDef extends CollectionIdentifierFieldDefinition

  val collectibleRegDef = new RegisterDefinition {
    override val fields = List(collectionFieldDef)
  }

  val collectibleRegCollectionId :Register.CollectionId = 23

  case class collectionField(override val value :Register.CollectionId) extends CollectionIdentifierField {
    override val definition = collectionFieldDef
  }

  val collectibleReg = new Register {
    override val fields = List(collectionField(collectibleRegCollectionId))
    override val definition = collectibleRegDef
  }

  val reg1ForeignKey = new ForeignKeyFieldDefinition {
    override val target = collectibleRegDef
  }

  val regDefinition2 = new RegisterDefinition {
    override val fields = List(reg1ForeignKey)
  }

  val reg3CollRefFieldDef = new CollectionReferenceFieldDefinition {
    override val target = collectionFieldDef
  }

  val regDefinition3 = new RegisterDefinition {
    override val fields = List(reg3CollRefFieldDef)
  }

  behavior of "A storage Manager"

  it should "throw an IllegalArgumentException if a duplicated register definition is entered" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(collectibleRegDef, collectibleRegDef))
    }
  }

  it should "throw an IllegalArgumentException if a duplicated collection definition is entered" in {
    an [IllegalArgumentException] should be thrownBy {
      val regDef = new RegisterDefinition {
        override val fields = Vector(collectionFieldDef, new FieldDefinition() {})
      }

      newStorageManager(List(collectibleRegDef, regDef))
    }
  }

  it should "throw an IllegalArgumentException if at least one of the given register definitions has a foreign key for a register that is not included in the list" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(regDefinition2))
    }
  }

  it should "throw an IllegalArgumentException if at least one of the given register definitions has a collection reference that is not included in the list" in {
    an [IllegalArgumentException] should be thrownBy {
      newStorageManager(List(regDefinition3))
    }
  }

  it should "allow register definitions containing a valid array index" in {
    val regDef = new RegisterDefinition() {
      override val fields = List(collectionFieldDef, new ArrayIndexFieldDefinition {
        override val collection = collectionFieldDef
      })
    }

    newStorageManager(List(regDef))
  }

  it should "throw an IllegalArgumentException if at least one of the given register definitions contains an array index field pointing outside the same register" in {
    an [IllegalArgumentException] should be thrownBy {
      val regDef = new RegisterDefinition() {
        override val fields = List(new ArrayIndexFieldDefinition {
          override val collection = collectionFieldDef
        })
      }

      newStorageManager(List(collectibleRegDef, regDef))
    }
  }

  it can "insert a register and retrieve it back with the given identifier" in {
    val storageManager = newStorageManager(List(collectibleRegDef))
    val keyOption = storageManager.insert(collectibleReg)
    keyOption shouldBe defined

    val regOption = storageManager.get(collectibleRegDef, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual collectibleReg
  }

  it can "return a value more than once for the same key" in {
    val storageManager = newStorageManager(List(collectibleRegDef))
    val keyOption = storageManager.insert(collectibleReg)
    keyOption shouldBe defined

    val regOption1 = storageManager.get(collectibleRegDef, keyOption.get)
    regOption1 shouldBe defined
    regOption1.get shouldEqual collectibleReg

    val regOption2 = storageManager.get(collectibleRegDef, keyOption.get)
    regOption2 shouldBe defined
    regOption2.get shouldEqual collectibleReg
  }

  it can "insert a register and delete it using the given identifier" in {
    val storageManager = newStorageManager(List(collectibleRegDef))
    val keyOption = storageManager.insert(collectibleReg)
    keyOption shouldBe defined

    storageManager.delete(collectibleRegDef, keyOption.get) shouldBe true
  }

  it can "not delete more than once for the same key" in {
    val storageManager = newStorageManager(List(collectibleRegDef))
    val keyOption = storageManager.insert(collectibleReg)
    keyOption shouldBe defined

    storageManager.delete(collectibleRegDef, keyOption.get) shouldBe true
    storageManager.delete(collectibleRegDef, keyOption.get) shouldBe false
  }

  it can "not insert a register pointing to nothing" in {
    val storageManager = newStorageManager(List(collectibleRegDef, regDefinition2))
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
    val storageManager = newStorageManager(List(collectibleRegDef, regDefinition2))
    val keyOption = storageManager.insert(collectibleReg)
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

    storageManager.delete(collectibleRegDef, keyOption.get) shouldBe false
    storageManager.delete(regDefinition2, keyOption2.get) shouldBe true
    storageManager.delete(collectibleRegDef, keyOption.get) shouldBe true
  }

  it should "return a null set before inserting anything" in {
    val manager = newStorageManager(List(collectibleRegDef))
    manager.getKeysFor(collectibleRegDef).isEmpty shouldBe true
  }

  it should "return only the key for the register inserted" in {
    val manager = newStorageManager(List(collectibleRegDef))
    val keyOption = manager.insert(collectibleReg)
    keyOption shouldBe defined

    manager.getKeysFor(collectibleRegDef).size shouldBe 1
    manager.getKeysFor(collectibleRegDef) should contain (keyOption.get)
  }

  it should "return only the keys for registers inserted (more than one)" in {
    val manager = newStorageManager(List(collectibleRegDef))
    val keyOption1 = manager.insert(collectibleReg)
    keyOption1 shouldBe defined

    val keyOption2 = manager.insert(collectibleReg)
    keyOption2 shouldBe defined

    manager.getKeysFor(collectibleRegDef).size shouldBe 2
    manager.getKeysFor(collectibleRegDef) should contain (keyOption1.get)
    manager.getKeysFor(collectibleRegDef) should contain (keyOption2.get)
  }

  it can "replace one register by another with the same definition" in {
    val storageManager = newStorageManager(List(collectibleRegDef))
    val keyOption = storageManager.insert(collectibleReg)
    keyOption shouldBe defined

    val regB = new Register {
      override val fields = List(collectionField(collectibleRegCollectionId + 1))
      override val definition = collectibleRegDef
    }
    regB should not equal collectibleReg

    storageManager.replace(regB, keyOption.get) shouldBe true

    val regOption = storageManager.get(collectibleRegDef, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual regB
  }

  it can "not replace one register by another if the key is not defined previously" in {
    val storageManager = newStorageManager(List(collectibleRegDef))
    val keyOption = storageManager.insert(collectibleReg)
    keyOption shouldBe defined

    val regB = new Register {
      override val fields = List(collectionField(collectibleRegCollectionId + 1))
      override val definition = collectibleRegDef
    }
    regB should not equal collectibleReg

    val newKey = keyOption.get + 100
    storageManager.replace(regB, newKey) shouldBe false

    val regOption = storageManager.get(collectibleRegDef, keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual collectibleReg

    storageManager.get(collectibleRegDef, newKey) shouldBe None
  }

  it should "return all register keys for those register with the given collection id" in {
    val manager = newStorageManager(List(collectibleRegDef))
    val ids = List[Register.CollectionId](1,2,3,1,2,1)
    val keys = for (id <- ids) yield {
      manager.insert(new Register {
        override val fields: Seq[Field] = List(collectionField(id))
        override val definition = collectibleRegDef
      }).get
    }

    for (id <- ids.min until ids.max) {
      val expected = ids zip keys filter { case (i,_) => i == id } map { x => x._2 }
      manager.getKeysForCollection(collectionFieldDef, id) shouldBe expected.toSet
    }
  }

  it should "return a map containing all inserted registers and grouped by their keys" in {
    val manager = newStorageManager(List(collectibleRegDef))
    val inserted = for (i <- 0 until 10) yield {
      val reg = new Register {
        override val fields = List(collectionField(i))
        override val definition: RegisterDefinition = collectibleRegDef
      }
      val result = manager.insert(reg).map(key => (key, reg))
      result shouldBe defined
      result.get
    }

    val map = manager.getMapFor(collectibleRegDef)
    map.size shouldBe inserted.size
    for (insertion <- inserted) {
      val opt = map.get(insertion._1)
      opt shouldBe defined
      opt.get shouldBe insertion._2
    }
  }

  it should "return a map for all registers with the given collection id" in {
    val manager = newStorageManager(List(collectibleRegDef))
    val ids = List[Register.CollectionId](1,2,3,1,2,1)
    val inserted = for (id <- ids) yield {
      val reg = new Register {
        override val fields = List(collectionField(id))
        override val definition = collectibleRegDef
      }
      val key = manager.insert(reg).get
      (key,reg)
    }

    for (id <- ids.min until ids.max) {
      val expected = inserted.filter { case (_, reg) =>
        reg.fields.head.value == id
      }
      val map = manager.getMapForCollection(collectionFieldDef, id)
      map.size shouldBe expected.size
      for (expectation <- expected) {
        val value = map.get(expectation._1)
        value shouldBe defined
        value.get shouldBe expectation._2
      }
    }
  }
}
