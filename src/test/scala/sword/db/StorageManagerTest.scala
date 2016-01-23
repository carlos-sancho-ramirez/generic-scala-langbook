package sword.db

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

    val regOption = storageManager.get(keyOption.get)
    regOption shouldBe defined
    regOption.get shouldEqual numReg
  }

  it can "return a value more than once for the same key" in {
    val storageManager = newStorageManager(List(numRegDef))
    val keyOption = storageManager.insert(numReg)
    keyOption shouldBe defined

    val regOption1 = storageManager.get(keyOption.get)
    regOption1 shouldBe defined
    regOption1.get shouldEqual numReg

    val regOption2 = storageManager.get(keyOption.get)
    regOption2 shouldBe defined
    regOption2.get shouldEqual numReg
  }

  it can "insert a register and delete it using the given identifier" in {
    val storageManager = newStorageManager(List(numRegDef))
    val keyOption = storageManager.insert(numReg)
    keyOption shouldBe defined

    storageManager.delete(keyOption.get) shouldBe true
  }

  it can "not delete more than once for the same key" in {
    val storageManager = newStorageManager(List(numRegDef))
    val keyOption = storageManager.insert(numReg)
    keyOption shouldBe defined

    storageManager.delete(keyOption.get) shouldBe true
    storageManager.delete(keyOption.get) shouldBe false
  }

  it can "not accept keys generated by another storage manager instance on calling the get method" in {
    val storageManagerA = newStorageManager(List(numRegDef))
    val storageManagerB = newStorageManager(List(numRegDef))
    val keyOption = storageManagerA.insert(numReg)
    keyOption shouldBe defined

    an [IllegalArgumentException] should be thrownBy {
      storageManagerB.get(keyOption.get)
    }
  }

  it can "not insert a register pointing to nothing" in {
    val storageManager = newStorageManager(List(numRegDef, numRegRefRegDef))
    val keyOpt = storageManager.insert(numReg)
    keyOpt shouldBe defined

    val numRegKey = keyOpt.get
    storageManager.delete(numRegKey) shouldBe true

    val reg2 = new Register {
      override val fields = List(new ForeignKeyField {
        override val key = numRegKey
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

    storageManager.delete(keyOption.get) shouldBe false
    storageManager.delete(keyOption2.get) shouldBe true
    storageManager.delete(keyOption.get) shouldBe true
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

  it can "insert more than one collection" in {
    val manager = newStorageManager(List(numRegDef))
    val reg1 = new NumRegister(5)
    val reg2 = new NumRegister(7)
    val reg3 = new NumRegister(23)
    val reg4 = new NumRegister(45)
    val reg5 = new NumRegister(58)

    val list1 = List(reg1, reg2, reg4)
    val list2 = List(reg3, reg4, reg5)
    val coll1IdOption = manager.insert(list1)
    coll1IdOption shouldBe defined
    val coll2IdOption = manager.insert(list2)
    coll2IdOption shouldBe defined

    val keys = manager.getKeysFor(reg1.definition)
    keys.size shouldBe (list1.size + list2.size)

    keys.filter(_.group == coll1IdOption.get).flatMap(manager.get(_)).toSet shouldBe list1.toSet
    keys.filter(_.group == coll2IdOption.get).flatMap(manager.get(_)).toSet shouldBe list2.toSet
  }

  it should "throw an UnsupportedOperationException in case of inserting a collection for non-collectible registers, and this operation should not change the storage state" in {
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

    manager.getKeysFor(myRegDef) shouldBe empty
  }

  it should "throw an UnsupportedOperationException in case of inserting a collection of different collectible registers definitions, and this operation should not change the storage state" in {
    val myRegDef = new CollectibleRegisterDefinition {
      override val fields = List(numFieldDef)
    }

    val manager = newStorageManager(List(numRegDef, myRegDef))
    class MyNumReg(value :Int) extends Register {
      override val definition = myRegDef
      override val fields = List(numField(value))
    }

    val reg1 = new NumRegister(5)
    val reg2 = new MyNumReg(7)

    val list = List(reg1, reg2)
    intercept[UnsupportedOperationException] {
      manager.insert(list)
    }

    manager.getKeysFor(numRegDef) shouldBe empty
    manager.getKeysFor(myRegDef) shouldBe empty
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

    val regOption = storageManager.get(keyOption.get)
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

    val badKey = keyOption.get
    storageManager.delete(badKey) shouldBe true
    storageManager.replace(regB, badKey) shouldBe false
  }

  it should "return all register keys for those register with the given collection id on calling getKeysForCollection" in {
    val manager = newStorageManager(List(numRegDef))
    val groups = List[Int](1,2,3,1,2,1)
    val collections = groups.indices.map(_ + 1).zip(groups).groupBy { case (_,group) => group }
        .flatMap { case (group, items) =>
      val regs = items.map { case (index,_) => new Register {
        override val fields = List(numField(index))
        override val definition = numRegDef
      }}

      manager.insert(regs).map(id => (group, id))
    }

    for (group <- groups.toSet[Int]) {
      val expected = groups.indices.zip(groups).filter { case (_,g) => g == group } map { x => x._1 + 1 }
      val keys = manager.getKeysForCollection(numRegDef, collections.find(_._1 == group).head._2)
      keys.flatMap(manager.get).map(_.fields.head.asInstanceOf[numField].value) shouldBe expected.toSet
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

  it should "return for a given collection identifier a map matching all registers with their keys" in {
    val manager = newStorageManager(List(numRegDef))
    val groups = List[Int](1,2,3,1,2,1,3)
    val collections = groups.indices.map(_ + 1).zip(groups).groupBy { case (_,group) => group }
      .flatMap { case (group, items) =>
        val regs = items.map { case (index,_) => new Register {
          override val fields = List(numField(index))
          override val definition = numRegDef
        }}

        manager.insert(regs).map(id => (group, id))
      }

    for (group <- groups.toSet[Int]) {
      val expected = groups.indices.zip(groups).filter { case (_,g) => g == group } map { x => x._1 + 1 }
      val regs = manager.getMapForCollection(numRegDef, collections.find(_._1 == group).head._2)
      regs.values.map(_.fields.head.asInstanceOf[numField].value).toSet shouldBe expected.toSet
    }
  }
}