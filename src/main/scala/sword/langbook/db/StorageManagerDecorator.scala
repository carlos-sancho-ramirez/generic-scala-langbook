package sword.langbook.db

class StorageManagerDecorator(override val wrapped :StorageManager) extends StorageManagerWrapper {

  def getKeysForSet(set: SetIdentifierFieldDefinition, id :Register.SetId) :Set[Register.Key] = {
    val regDef = registerDefinitions.find(_.fields.contains(set)).get
    getKeysFor(regDef).flatMap { key =>
      get(regDef, key).filter { reg =>
        reg.fields.collectFirst { case x: SetIdentifierField if x.definition == set && x.value == id =>
          x
        }.nonEmpty
      }.map(_ => key)
    }
  }
}
