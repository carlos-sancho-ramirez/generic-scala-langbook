package sword.langbook.db

import sword.db.{UnicodeField, StorageManager}

case class Symbol(key :StorageManager.Key) {
  def fields = key.registerOption.map(_.fields).getOrElse(Seq())
  def unicodeOpt = fields.collectFirst {
    case field :UnicodeField => field.value
  }

  def unicode = unicodeOpt.get
}
