module Main exposing (..)

import Html exposing (Html, div, table, tr, td, p, text, h1)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import CsvDecode exposing ((|=))
import CsvDecode
import LocalStorage as LS

---- MODEL ----


type alias Record =
    { id : String
    , name : String
    }

type alias Model =
    Result String (List Record)


init : ( Model, Cmd Msg )
init = Err "Not loaded." ! [LS.getDeckReq "DEV-1"]


---- UPDATE ----


type Msg
    = NoOp
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
        NoOp ->
            model ! []

        ItemSet () ->
            let
                _ = Debug.log "setItem succeeded." ()
            in
                model ! []

        ItemGet valueMaybe ->
            case valueMaybe of
                Nothing ->
                    Err "getItem failed."! []

                Just records ->
                    Ok records ! []


---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Your Elm App is working!"]
        , case model of
            Ok records ->
                table [] <|
                    List.map (\{ id, name } ->
                        Html.tr []
                        [ td [] [ text id ]
                        , td [] [ text name ]
                        ]) records

            Err msg ->
                p [] [text msg]
        ]


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


buildFromCSV : String -> Model
buildFromCSV csv =
    let
        decoder =
            CsvDecode.succeed Record
                |= CsvDecode.field "id"
                |= CsvDecode.field "name"
    in
        CsvDecode.run decoder csv
