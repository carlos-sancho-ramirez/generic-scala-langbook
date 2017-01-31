package sword.langbook.db

import sword.db.StorageManager

/**
 * Base trait for all items in the linked layer that are expected to appear in a Selector
 */
trait Selectable {

  def key: StorageManager.Key

  /**
   * Return the suitable human readable string for this element.
   */
  def suitableText: Option[String]
}
