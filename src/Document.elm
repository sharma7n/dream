module Document exposing
    ( DocumentItemData
    , Document
    , DocumentLine(..)
    , parseDocument
    , ParseDocumentError(..)
    )

type alias DocumentItemData =
    { text : String
    , document : Document
    , workingText : Maybe String
    }

type alias Document =
    { lines : List DocumentLine
    }

type DocumentLine
    = BlankDocumentLine
    | Header1DocumentLine String
    | ParagraphDocumentLine String

parseDocument : String -> Result ( List ParseDocumentError ) Document
parseDocument rawText =
    Ok <|
        { lines =
            [ ParagraphDocumentLine rawText
            ]
        }

type ParseDocumentError
    = ParseDocumentError