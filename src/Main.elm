module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Random exposing (Generator)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import UUID exposing (UUID)

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

unambiguousName : Named a -> String
unambiguousName named =
    let
        firstName =
            named.names
                |> List.head
    in
    case firstName of
        Nothing ->
            "<anonymous> @ " ++ String.slice 0 6 named.id
        
        Just name ->
            case name of
                "" ->
                    "<anonymous> @ " ++ String.slice 0 6 named.id
                
                _ ->
                    name


type alias Item =
    { id : String
    , names : List String
    , workingName : Maybe String
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
        initModel : Model
        initModel =
            { items = Dict.empty
            , bundle = []
            }
    in
    ( initModel, Cmd.none )



---- UPDATE ----


type Msg
    = UserViewItem String
    | UserEditItem String
    | UserEditDocumentText String String
    | UserSaveEdit String
    | UserCancelEdit String
    | UserAddDocumentItem
    | UserDeleteDocumentItem String
    | SystemGotDocumentItemUUID UUID
    | UserSetItemName String String


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
        
        UserSaveEdit id ->
            let
                saveEdit : Item -> Item
                saveEdit item =
                    let
                        newNames =
                            case item.workingName of
                                Nothing ->
                                    item.names
                                
                                Just commitName ->
                                    commitName :: item.names

                    in
                    { item
                        | names = newNames
                        , workingName = Nothing
                        , mode = ViewItemMode
                        , details = saveItemDetails item.details
                    }
            in
            updateItem id saveEdit model
        
        UserCancelEdit id ->
            let
                cancelEdit : Item -> Item
                cancelEdit item =
                    { item
                        | workingName = Nothing
                        , mode = ViewItemMode
                        , details = cancelItemDetails item.details
                    }
            in
            updateItem id cancelEdit model
        
        UserAddDocumentItem ->
            let
                newCmd =
                    Random.generate SystemGotDocumentItemUUID UUID.generator
            in
            ( model, newCmd )
        
        UserDeleteDocumentItem id ->
            let
                newModel =
                    { model
                        | items =
                            model.items
                                |> Dict.remove id
                        , bundle =
                            model.bundle
                                |> List.filter (\s -> s /= id)
                    }
            in
            ( newModel, Cmd.none )
        
        SystemGotDocumentItemUUID uuid ->
            let
                id = UUID.toString uuid

                newItem =
                    { id = id
                    , names = []
                    , workingName = Nothing
                    , mode = EditItemMode
                    , details =
                        DocumentItem
                            { text = ""
                            , workingText = Nothing
                            }
                    }
                
                newModel =
                    { model
                        | items =
                            model.items
                                |> Dict.insert id newItem
                        , bundle =
                            model.bundle ++ [ id ]
                    }
            in
            ( newModel, Cmd.none )
        
        UserSetItemName id newName ->
            let
                setWorkingItemName : Item -> Item
                setWorkingItemName item =
                    { item
                        | workingName = Just newName
                    } 
                
                newModel =
                    { model
                        | items =
                            model.items
                                |> Dict.update id ( Maybe.map setWorkingItemName )
                    }
            in
            ( newModel, Cmd.none )


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

saveItemDetails : ItemDetails -> ItemDetails
saveItemDetails details =
    case details of
        MissingItem ->
            details
        
        DocumentItem data ->
            DocumentItem
                { data
                    | text =
                        data.workingText
                            |> Maybe.withDefault data.text
                    , workingText = Nothing
                }

cancelItemDetails : ItemDetails -> ItemDetails
cancelItemDetails details =
    case details of
        MissingItem ->
            details
        
        DocumentItem data ->
            DocumentItem
                { data
                    | text = data.text
                    , workingText = Nothing
                }

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
            ( [ Element.Input.button
                [ Element.Background.color <| Element.rgb255 75 150 75
                , Element.Font.color <| Element.rgb255 225 225 225
                , Element.padding 5
                , Element.Font.bold
                ]
                { onPress = Just <| UserAddDocumentItem
                , label = Element.text "Add Document"
                }
              ]
              ++ List.map ( viewItemReference model ) model.bundle

            )
        )

viewItemReference : Model -> String -> Element Msg
viewItemReference model ref =
    let
        item : Named Item
        item =
            model.items
                |> Dict.get ref
                |> Maybe.withDefault
                    { id = ""
                    , names = []
                    , workingName = Nothing
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
                    "📄 Document"
        
        fullyQualifiedName : String
        fullyQualifiedName =
            [ itemClassName
            , ":"
            , unambiguousName item
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
                    , Element.Input.button
                        [ Element.alignRight
                        ]
                        { onPress = Just <| UserDeleteDocumentItem item.id
                        , label = Element.text "❌"
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
                    [ Element.Input.text
                        []
                        { onChange = UserSetItemName item.id
                        , text =
                            case item.workingName of
                                Nothing ->
                                    item.names
                                        |> List.head
                                        |> Maybe.withDefault ""
                                
                                Just name ->
                                    name
                        , placeholder = Nothing
                        , label =
                            Element.Input.labelLeft
                                []
                                ( Element.text "Item Name" )
                        }
                    , Element.Input.button
                        [ Element.alignRight
                        ]
                        { onPress = Just <| UserDeleteDocumentItem item.id
                        , label = Element.text "❌"
                        }
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
                        { onPress = Just <| UserSaveEdit item.id
                        , label = Element.text "Save"
                        }
                    , Element.Input.button
                        [ Element.Background.color <| Element.rgb255 150 75 75
                        , Element.Font.color <| Element.rgb255 225 225 225
                        , Element.padding 5
                        , Element.Font.bold
                        ]
                        { onPress = Just <| UserCancelEdit item.id
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
                ( Element.none )
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
