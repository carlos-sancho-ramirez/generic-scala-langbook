package sword.langbook

import sword.db._
import sword.langbook.db.LinkedStorageManager

object Main {

  /**
   * Sample using the storageManager for writing values and the LinkedStorageManager to retrieve
   * them.
   */
  def main(args :Array[String]) :Unit = {
    val manager = LinkedStorageManager(regDefs => new MemoryStorageManager(regDefs))

    val spanishConcept = new Register {
      override val definition = db.registers.Concept
      override val fields = List(CharSequenceField("Spanish"))
    }
    val spanishConceptKey = manager.storageManager.insert(spanishConcept).get

    val spanishLanguage = new Register {
      override val definition = db.registers.Language
      override val fields = List(new ForeignKeyField {
        override val key = spanishConceptKey
        override val definition = db.registers.Language.fields.head
      })
    }
    val spanishLanguageKey = manager.storageManager.insert(spanishLanguage).get

    val romanAlphabetConcept = new Register {
      override val definition = db.registers.Concept
      override val fields = List(CharSequenceField("roman alphabet"))
    }
    val romanAlphabetConceptKey = manager.storageManager.insert(romanAlphabetConcept).get

    val romanAlphabet = new Register {
      override val definition = db.registers.Alphabet
      override val fields = List(new ForeignKeyField {
        override val key = romanAlphabetConceptKey
        override val definition = db.registers.Alphabet.fields.head
      })
    }
    val romanAlphabetKey = manager.storageManager.insert(romanAlphabet).get

    val letterConcept = new Register {
      override val definition = db.registers.Concept
      override val fields = List(CharSequenceField("letter"))
    }
    val letterConceptKey = manager.storageManager.insert(letterConcept).get

    val symbolA = new Register {
      override val definition = db.registers.Symbol
      override val fields = List(UnicodeField(0x61))
    }
    val symbolAKey = manager.storageManager.insert(symbolA).get

    val symbolC = new Register {
      override val definition = db.registers.Symbol
      override val fields = List(UnicodeField(0x63))
    }
    val symbolCKey = manager.storageManager.insert(symbolC).get

    val symbolR = new Register {
      override val definition = db.registers.Symbol
      override val fields = List(UnicodeField(0x62))
    }
    val symbolRKey = manager.storageManager.insert(symbolR).get

    val symbolT = new Register {
      override val definition = db.registers.Symbol
      override val fields = List(UnicodeField(0x64))
    }
    val symbolTKey = manager.storageManager.insert(symbolT).get

    def symbolRefReg(k :StorageManager.Key) = new Register {
      override val definition = db.registers.SymbolPosition
      override val fields = List(new ForeignKeyField {
        override val key = k
        override val definition = db.registers.SymbolPosition.fields.head
      })
    }

    val symbolArrayCollOpt = manager.storageManager.insert(List(
        symbolRefReg(symbolCKey),
        symbolRefReg(symbolAKey),
        symbolRefReg(symbolRKey),
        symbolRefReg(symbolTKey),
        symbolRefReg(symbolAKey)))

    val cartaPiece = new Register {
      override val definition = db.registers.Piece
      override val fields = List(
        new ForeignKeyField {
          override val key = romanAlphabetKey
          override val definition = db.registers.Piece.fields.head.asInstanceOf[ForeignKeyFieldDefinition]
        },
        new CollectionReferenceField {
          override val definition = db.registers.Piece.fields(1).asInstanceOf[CollectionReferenceFieldDefinition]
          override val collectionId = symbolArrayCollOpt.get
        }
      )
    }
    val cartaPieceCollId = manager.storageManager.insert(List(cartaPiece)).get

    val cartaPieceArray = new Register {
      override val definition = db.registers.PiecePosition
      override val fields = List(new CollectionReferenceField {
        override val collectionId = cartaPieceCollId
        override val definition = db.registers.PiecePosition.fields.head
      })
    }
    val cartaPieceArrayId = manager.storageManager.insert(List(cartaPieceArray)).get

    val cartaWord = new Register {
      override val definition = db.registers.Word
      override val fields = List(
        new ForeignKeyField {
          override val key = spanishLanguageKey
          override val definition = db.registers.Word.fields.head.asInstanceOf[ForeignKeyFieldDefinition]
        },
        new CollectionReferenceField {
          override val definition = db.registers.Word.fields(1).asInstanceOf[CollectionReferenceFieldDefinition]
          override val collectionId = cartaPieceArrayId
        }
      )
    }
    manager.storageManager.insert(cartaWord)

    for {
      language <- manager.languages.values
      alphabet <- manager.alphabets.values
      word <- manager.words.values
    } {
      val text = word.pieces.flatMap(_.get(alphabet)).flatMap(x => x).map(_.unicode.toChar).mkString("")
      println(s"Language: $language, Alphabet: $alphabet => $text")
    }
  }
}

