module Main exposing (..)

import Html exposing (Html, div, table, tr, td, p, text, h1)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import CsvDecode exposing ((|=))
import CsvDecode
import LocalStorage as LS
import Json.Encode exposing (Value, list, string, object)

---- MODEL ----


type alias Record =
    { id : String
    , name : String
    }

type alias Model =
    Result String (List Record)


init : ( Model, Cmd Msg )
init =
    let
        csv_data = """
id,name
DEV-1,This is name
DEV-2,名前
"""
        model = buildFromCSV csv_data
    in
        model ! case model of
            Ok data -> [LS.setItemReq ("DEV-1", toValue data)]
            Err msg -> [LS.setItemReq ("DEV-0", string "ERROR")]


toValue : List Record -> Value
toValue model =
    let
        contents =
            List.map (\{id, name} ->
                object
                    [ ("id", string id)
                    , ("name", string name)
                    ]
                ) model
    in
        list contents

---- UPDATE ----


type Msg
    = NoOp
    | ItemSet ()
    | ItemGet (Maybe Value)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ LS.setItemRes ItemSet
        , LS.getItemRes ItemGet
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []
    {-
    case msg of
        RecordLoaded recordList ->
            recordList ! []
    -}

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
