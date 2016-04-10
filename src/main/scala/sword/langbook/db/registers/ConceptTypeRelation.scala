package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._

object ParentTypeConceptReferenceFieldDefinition extends ForeignKeyFieldDefinition {
  override val target = Concept
  override def from(value: String, keyExtractor: String => Option[StorageManager.Key]) = {
    keyExtractor(value).map(ParentTypeConceptReferenceField)
  }
}

case class ParentTypeConceptReferenceField(override val key :StorageManager.Key) extends ForeignKeyField {
  override val definition = ParentTypeConceptReferenceFieldDefinition
  override def toString = key.toString
}

object ConceptTypeRelation extends RegisterDefinition[ConceptTypeRelation] {
  override val fields = Vector(ConceptReferenceFieldDefinition, ParentTypeConceptReferenceFieldDefinition)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      val conceptKey = keyExtractor(ConceptReferenceFieldDefinition)(values.head)
      val parentTypeConceptKey = keyExtractor(ParentTypeConceptReferenceFieldDefinition)(values(1))
      if (conceptKey.isDefined && parentTypeConceptKey.isDefined) {
        Some(ConceptTypeRelation(conceptKey.get, parentTypeConceptKey.get))
      }
      else None
    }
    else None
  }
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
