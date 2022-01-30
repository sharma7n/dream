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
    { id : String
    , names : List String
    , mode : ItemMode
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
    , workingText : Maybe String
    }

mapDocumentItem : ( DocumentItemData -> DocumentItemData ) -> Item -> Item
mapDocumentItem f item =
    case item.details of
        DocumentItem data ->
            let
                newDetails =
                    DocumentItem ( f data )
                
                newItem =
                    { item
                        | details = newDetails
                    }
            in
            newItem
        
        _ ->
            item

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
                    , workingText = Nothing
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
    = UserViewItem String
    | UserEditItem String
    | UserEditDocumentText String String
    | UserSaveDocumentEdit String
    | UserCancelDocumentEdit String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserViewItem id ->
            let
                setViewMode : Item -> Item
                setViewMode item =
                    { item | mode = ViewItemMode }
            in
            updateItem id setViewMode model
            
        UserEditItem id ->
            let
                setEditMode : Item -> Item
                setEditMode item =
                    { item | mode = EditItemMode }
            in
            updateItem id setEditMode model
        
        UserEditDocumentText id newText ->
            let
                setDocumentText : Item -> Item
                setDocumentText item =
                    let
                        f data =
                            { data
                                | workingText = Just newText
                            }
                    in
                    mapDocumentItem f item
            in
            updateItem id setDocumentText model
        
        UserSaveDocumentEdit id ->
            let
                saveDocumentEdit : Item -> Item
                saveDocumentEdit item =
                    let
                        f data =
                            { data
                                | text =
                                    data.workingText
                                        |> Maybe.withDefault data.text
                                , workingText = Nothing
                            }
                    in
                    mapDocumentItem f item
                        |> (\i -> { i | mode = ViewItemMode })
            in
            updateItem id saveDocumentEdit model
        
        UserCancelDocumentEdit id ->
            let
                cancelDocumentEdit : Item -> Item
                cancelDocumentEdit item =
                    let
                        f data =
                            { data
                                | text = data.text
                                , workingText = Nothing
                            }
                    in
                    mapDocumentItem f item
                        |> (\i -> { i | mode = ViewItemMode })
            in
            updateItem id cancelDocumentEdit model


updateItem : String -> ( Item -> Item ) -> Model -> ( Model, Cmd msg )
updateItem id f model =
    let
        newModel =
            { model
                | items =
                    model.items
                        |> Dict.update id ( Maybe.map f )
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

viewItemReference : Dict String Item -> String -> Element Msg
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
                        , Element.Font.bold
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
                    ( viewViewItemDetails item.details )
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
                        , Element.Font.bold
                        ]
                        ( Element.text fullyQualifiedName )
                    ]
                , Element.el
                    [ Element.padding 5
                    , Element.width <| Element.fill
                    ]
                    ( viewEditItemDetails item.id item.details )
                , Element.row
                    [ Element.padding 5
                    , Element.spacing 10
                    ]
                    [ Element.Input.button
                        [ Element.Background.color <| Element.rgb255 75 150 75
                        , Element.Font.color <| Element.rgb255 225 225 225
                        , Element.padding 5
                        , Element.Font.bold
                        ]
                        { onPress = Just <| UserSaveDocumentEdit item.id
                        , label = Element.text "Save"
                        }
                    , Element.Input.button
                        [ Element.Background.color <| Element.rgb255 150 75 75
                        , Element.Font.color <| Element.rgb255 225 225 225
                        , Element.padding 5
                        , Element.Font.bold
                        ]
                        { onPress = Just <| UserCancelDocumentEdit item.id
                        , label = Element.text "Cancel"
                        }
                    ]
                ]

viewViewItemDetails : ItemDetails -> Element msg
viewViewItemDetails details =
    case details of
        MissingItem ->
            Element.none
        
        DocumentItem data ->
            viewViewDocumentItemData data

viewViewDocumentItemData : DocumentItemData -> Element msg
viewViewDocumentItemData data =
    Element.text data.text

viewEditItemDetails : String -> ItemDetails -> Element Msg
viewEditItemDetails id details =
    case details of
        MissingItem ->
            Element.none
        
        DocumentItem data ->
            viewEditDocumentItemData id data

viewEditDocumentItemData : String -> DocumentItemData -> Element Msg
viewEditDocumentItemData id data =
    Element.Input.multiline
        []
        { onChange = UserEditDocumentText id
        , text =
            data.workingText
                |> Maybe.withDefault data.text
        , placeholder = Nothing
        , label =
            Element.Input.labelAbove
                []
                ( Element.text "Document text" )
        , spellcheck = True
        }

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
