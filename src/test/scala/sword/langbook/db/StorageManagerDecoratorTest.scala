package sword.langbook.db

import org.scalatest.{Matchers, FlatSpec}

class StorageManagerDecoratorTest extends FlatSpec with Matchers {

  def newStorageManagerDecorator(definitions :Seq[RegisterDefinition]) =
      new StorageManagerDecorator(new MemoryStorageManager(definitions))

  object setFieldDef extends SetIdentifierFieldDefinition

  val regDefWithSet = new RegisterDefinition {
    override val fields = List(setFieldDef)
  }

  behavior of "The Storage Manager Decorator"

  it should "return all register keys for those register with the given set id" in {
    val manager = newStorageManagerDecorator(List(regDefWithSet))
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
}
