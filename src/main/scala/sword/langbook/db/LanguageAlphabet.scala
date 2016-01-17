package sword.langbook.db

import sword.db.{ForeignKeyField, ForeignKeyFieldDefinition, Register}

object LanguageAlphabet {
  val alphabetKeyExtractor :(Register) => Register.Key = {
    val fields = registers.LanguageAlphabet.fields
    val index = fields.indices.zip(fields).collectFirst {
      case (index, fieldDef :ForeignKeyFieldDefinition) if fieldDef.target == registers.Alphabet => index
    }.get

    _.fields(index).asInstanceOf[ForeignKeyField].key
  }

  val languageKeyExtractor :(Register) => Register.Key = {
    val fields = registers.LanguageAlphabet.fields
    val index = fields.indices.zip(fields).collectFirst {
      case (index, fieldDef :ForeignKeyFieldDefinition) if fieldDef.target == registers.Language => index
    }.get

    _.fields(index).asInstanceOf[ForeignKeyField].key
  }
}
