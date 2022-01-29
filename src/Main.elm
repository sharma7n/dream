module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font


---- MODEL ----


type alias Model =
    { items : Dict String ( Named Item )
    , bundle : List String
    }

type alias Named a =
    { id : String
    , names : List String
    , value : a
    }

type Item
    = MissingItem
    | DocumentItem DocumentItemData

type alias DocumentItemData =
    { text : String
    }

init : ( Model, Cmd Msg )
init =
    let
        initItem : Named Item
        initItem =
            { id = "1"
            , names = []
            , value = DocumentItem
                { text = "Hello, world!"
                }
            }
        
        initModel : Model
        initModel =
            { items =
                Dict.fromList
                    [ ( "1", initItem)
                    ]
            , bundle =
                [ "1"
                ]
            }
    in
    ( initModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.Font.family
            [ Element.Font.monospace
            ]
        ]
        ( Element.column
            []
            ( List.map ( viewItemReference model.items ) model.bundle )
        )

viewItemReference : Dict String ( Named Item ) -> String -> Element msg
viewItemReference items ref =
    let
        item : Named Item
        item =
            items
                |> Dict.get ref
                |> Maybe.withDefault
                    { id = ""
                    , names = []
                    , value = MissingItem
                    }
        
        readableName : String
        readableName =
            item.names
                |> List.head
                |> Maybe.withDefault "<anonymous>"
        
        itemClassName : String
        itemClassName =
            case item.value of
                MissingItem ->
                    "MISSING"
                
                DocumentItem _ ->
                    "Document"
        
        fullyQualifiedName : String
        fullyQualifiedName =
            [ itemClassName
            , ":"
            , readableName
            , "@"
            , item.id
            ]
                |> String.join " "
    in
    Element.column
        [ Element.Font.alignLeft
        , Element.Border.color <| Element.rgb255 0 0 0
        , Element.Border.width 1
        ]
        [ Element.el
            [ Element.padding 5
            , Element.width <| Element.fill
            , Element.Background.color <| Element.rgb255 75 75 125
            , Element.Font.color <| Element.rgb255 225 225 225
            ]
            ( Element.text fullyQualifiedName )
        , Element.el
            [ Element.padding 5
            , Element.width <| Element.fill
            ]
            ( viewItemValue item.value )
        ]

viewItemValue : Item -> Element msg
viewItemValue item =
    case item of
        MissingItem ->
            Element.none
        
        DocumentItem data ->
            viewDocumentItemData data

viewDocumentItemData : DocumentItemData -> Element msg
viewDocumentItemData data =
    Element.text data.text


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
