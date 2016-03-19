package sword.langbook.db

import sword.db.{Register, StorageManager}

/**
  * Common base for all element factories.
  * @tparam R Register type to be used as input to generate the new element
  * @tparam E Element type that will be returned.
  */
trait ElementFactory[R <: Register, E] {

  def apply(key: StorageManager.Key): E

  def from(manager: LinkedStorageManager, register: R): Option[E] = {
    manager.storageManager.insert(register).map(apply)
  }
}
