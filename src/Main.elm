module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input

import Ui

---- MODEL ----


type alias Model =
    { items : Dict String ( Named Item )
    , bundle : List String
    }

type alias Named a =
    { a
        | id : String
        , names : List String
    }

type alias Item =
    { mode : ItemMode
    , details : ItemDetails
    }

type ItemMode
    = ViewItemMode
    | EditItemMode

type ItemDetails
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
            , mode = ViewItemMode
            , details =
                DocumentItem
                    { text = "Hello, world!"
                    }
            }
        
        initModel : Model
        initModel =
            { items =
                Dict.fromList
                    [ ( "1", initItem )
                    ]
            , bundle =
                [ "1"
                ]
            }
    in
    ( initModel, Cmd.none )



---- UPDATE ----


type Msg
    = UserEditItem String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserEditItem id ->
            let
                setEditMode : Named Item -> Named Item
                setEditMode item =
                    { item | mode = EditItemMode }
                
                newModel =
                    { model
                        | items =
                            model.items
                                |> Dict.update id (Maybe.map setEditMode)
                    }
            in
            ( newModel, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.Font.family
            [ Element.Font.monospace
            ]
        ]
        ( Element.column
            [ Element.padding 10
            , Element.spacing 10
            ]
            ( List.map ( viewItemReference model.items ) model.bundle )
        )

viewItemReference : Dict String ( Named Item ) -> String -> Element Msg
viewItemReference items ref =
    let
        item : Named Item
        item =
            items
                |> Dict.get ref
                |> Maybe.withDefault
                    { id = ""
                    , names = []
                    , mode = ViewItemMode
                    , details = MissingItem
                    }
        
        readableName : String
        readableName =
            item.names
                |> List.head
                |> Maybe.withDefault "<anonymous>"
        
        itemClassName : String
        itemClassName =
            case item.details of
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
    case item.mode of
        ViewItemMode ->
            Ui.cell
                [ Element.row
                    [ Element.padding 5
                    , Element.width <| Element.fill
                    , Element.Background.color <| Element.rgb255 75 75 75
                    , Element.Font.color <| Element.rgb255 225 225 225
                    ]
                    [ Element.el
                        [ Element.alignLeft
                        ]
                        ( Element.text fullyQualifiedName )
                    , Element.Input.button
                        [ Element.alignRight
                        ]
                        { onPress = Just <| UserEditItem item.id
                        , label = Element.text "✏️"
                        }
                    ]
                , Element.el
                    [ Element.padding 5
                    , Element.width <| Element.fill
                    ]
                    ( viewItemDetails item.details )
                ]
        
        EditItemMode ->
            Ui.cell
                [ Element.row
                    [ Element.padding 5
                    , Element.width <| Element.fill
                    , Element.Background.color <| Element.rgb255 75 75 75
                    , Element.Font.color <| Element.rgb255 225 225 225
                    ]
                    [ Element.el
                        [ Element.alignLeft
                        ]
                        ( Element.text fullyQualifiedName )
                    ]
                , Element.el
                    [ Element.padding 5
                    , Element.width <| Element.fill
                    ]
                    ( viewItemDetails item.details )
                , Element.row
                    []
                    [ Element.Input.button
                        []
                        { onPress = Nothing
                        , label = Element.text "Save"
                        }
                    , Element.Input.button
                        []
                        { onPress = Nothing
                        , label = Element.text "Cancel"
                        }
                    ]
                ]

viewItemDetails : ItemDetails -> Element msg
viewItemDetails details =
    case details of
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
