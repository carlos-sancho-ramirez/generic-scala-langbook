package sword.langbook.db.registers

import sword.db.StorageManager.Key
import sword.db._
import sword.langbook.db.registers.Alphabet.ConceptReferenceField

object ConceptTypeRelation extends RegisterDefinition[ConceptTypeRelation] {
  object SpecificConceptReferenceField extends ConceptReferenceFieldDefinition {
    override def newField = apply
  }
  case class SpecificConceptReferenceField(override val key: Key) extends AbstractConceptReferenceField {
    override val definition = SpecificConceptReferenceField
  }

  object GenericConceptReferenceField extends ConceptReferenceFieldDefinition {
    override def newField = apply
  }
  case class GenericConceptReferenceField(override val key: Key) extends AbstractConceptReferenceField {
    override val definition = GenericConceptReferenceField
  }

  override val fields = Vector(SpecificConceptReferenceField, GenericConceptReferenceField)
  override def from(values: Seq[String],
    keyExtractor: FieldDefinition => String => Option[Key]) = {
    if (values.size == fields.size) {
      for {
        specificConceptKey <- keyExtractor(SpecificConceptReferenceField)(values.head)
        genericConceptKey <- keyExtractor(GenericConceptReferenceField)(values(1))
      } yield {
        ConceptTypeRelation(specificConceptKey, genericConceptKey)
      }
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
 * @param specificConcept the concept that is a kind of parentTypeConcept. So, "A" in the previous definition.
 * @param genericConcept The generic concept including "concept". So, "B" in the previous definition.
 */
case class ConceptTypeRelation(specificConcept: StorageManager.Key, genericConcept: StorageManager.Key) extends Register {
  override val definition = ConceptTypeRelation
  override val fields = Vector(
    ConceptTypeRelation.SpecificConceptReferenceField(specificConcept),
    ConceptTypeRelation.GenericConceptReferenceField(genericConcept)
  )
}
