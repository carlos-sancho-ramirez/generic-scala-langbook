package sword.langbook.db.registers

import sword.db._

object ParentTypeConceptReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  def target = Concept
}

case class ParentTypeConceptReferenceField(override val key :StorageManager.Key) extends ForeignKeyField {
  override val definition = ParentTypeConceptReferenceFieldDefinition
  override def toString = key.toString
}

object ConceptTypeRelation extends RegisterDefinition {
  override val fields = Vector(ConceptReferenceFieldDefinition, ParentTypeConceptReferenceFieldDefinition)
}

/**
 * Add semantic relationship between concepts indicating that A is (a type of) B.
 * Note that this do not imply that B is type of A.
 *
 * E.g. "Dog" is an/a kind of "Animal", but "Animal" is not necessarily a "Dog"
 *
 * @param concept the concept that is a kind of parentTypeConcept. So, "A" in the previous definition.
 * @param parentTypeConcept The generic concept including "concept". So, "B" in the previous definition.
 */
case class ConceptTypeRelation(concept: StorageManager.Key, parentTypeConcept: StorageManager.Key) extends Register {
  override val definition = ConceptTypeRelation
  override val fields = Vector(ConceptReferenceField(concept), ParentTypeConceptReferenceField(parentTypeConcept))
}
