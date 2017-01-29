package sword.langbook.db

import sword.db.{ForeignKeyField, Register, StorageManager}

import scala.collection.Set

case class Bunch(key: StorageManager.Key) {
  private def regOpt = key.storageManager.get(key).map(_.asInstanceOf[registers.Bunch])
  def name = regOpt.get.name

  override def equals(other: Any) = {
    other.isInstanceOf[Bunch] && key == other.asInstanceOf[Bunch].key
  }
}

object Bunch extends ElementFactory[registers.Bunch, Bunch] {
  def from(manager: LinkedStorageManager, name: String): Option[Bunch] = {
    from(manager, registers.Bunch(name))
  }
}
