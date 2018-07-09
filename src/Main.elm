module Main exposing (..)

import Html exposing (Html, div, table, tr, td, p, text, h1)
import Html
import Html.Attributes exposing (class)
import Html.Attributes as Attr
import Html.Events as Events
import CsvDecode exposing ((|=))
import CsvDecode
import LocalStorage as LS
import FileReader exposing (NativeFile)
import FileReader.FileDrop as DZ
import Task

---- MODEL ----


type alias Record =
    { id : String
    , name : String
    }

type alias Model =
    { deck : Result String (List Record)
    , dragHovering : Int
    }


init : ( Model, Cmd Msg )
init = Model (Err "Not loaded.") 0 ! [LS.getDeckReq "DEV-1"]


---- UPDATE ----


type Msg
    = NoOp
    | OnDragEnter Int
    | OnDrop (List NativeFile)
    | FileContentsGot (Result FileReader.Error String)
    | ItemSet ()
    | ItemGet (Maybe (List Record))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ LS.setDeckRes ItemSet
        , LS.getDeckRes ItemGet
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ItemSet () ->
            let
                _ = Debug.log "setItem succeeded." ()
            in
                model ! []

        ItemGet valueMaybe ->
            case valueMaybe of
                Nothing ->
                    {model | deck = Err "getItem failed."} ! []

                Just records ->
                    {model | deck = Ok records} ! []

        OnDrop [file] ->
            model ! [getFileContents file]

        FileContentsGot (Ok csv) ->
            let
                deck = buildFromCSV csv
            in
                {model | deck = deck} ! case deck of
                    Ok deck ->
                        [LS.setDeckReq ("DEV-1", deck)]

                    Err _ ->
                        []

        _ -> model ! []


getFileContents : NativeFile -> Cmd Msg
getFileContents file =
    FileReader.readAsTextFile file.blob
        |> Task.attempt FileContentsGot

---- VIEW ----


view : Model -> Html Msg
view model =
    let
        dropZoneClass = dropZoneClass_ model.dragHovering
    in
        div []
            [ h1 [] [ text "Your Elm App is working!"]
            , div dropZoneClass
                [ Html.input
                    [ Attr.type_ "file"
                    , FileReader.onFileChange OnDrop
                    , Attr.multiple False
                    ] []
                ]
            , case model.deck of
                Ok records ->
                    deckTable records

                Err msg ->
                    p [] [text msg]
            ]


dropZoneClass_ : Int -> List (Html.Attribute Msg)
dropZoneClass_ dragHovering =
    let
        dzAttrs = DZ.dzAttrs (OnDragEnter 1) (OnDragEnter -1) NoOp OnDrop
    in
        if dragHovering > 0 then
            class "drop-zone active" :: dzAttrs
        else
            class "drop-zone" :: dzAttrs


deckTable : List Record -> Html Msg
deckTable records =
    let
        cells =
            records
            |> List.map (\{ id, name } ->
                tr []
                    [ td [] [ text id ]
                    , td [] [ text name ]
                    ])
    in
        table [] cells


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


buildFromCSV : String -> Result String (List Record)
buildFromCSV csv =
    let
        decoder =
            CsvDecode.succeed Record
                |= CsvDecode.field "ID"
                |= CsvDecode.field "name"
    in
        CsvDecode.run decoder csv
