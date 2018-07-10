module Main exposing (..)

import Dict exposing (Dict)
import FileReader exposing (NativeFile)
import FileReader.FileDrop as DZ
import Html
import Html exposing (Html, div, table, tr, td, h1, p, text)
import Html.Attributes as Attr
import Html.Attributes exposing (class)
import Html.Events as Events
import LocalStorage as LS
import Task
import Random

---- MODEL ----


type alias DeckSource =
    List (List (String, String))

type alias Deck =
    List (Dict String String)

type alias Model =
    { deck : Deck
    , message : String
    }


init : ( Model, Cmd Msg )
init = Model [] "Deck is not loaded." ! [LS.getDeckReq "DEV-1"]


---- UPDATE ----


type Msg
    = NoOp
    | OnDrop (List NativeFile)
    | FileContentsGot (Result FileReader.Error String)
    | ItemSet ()
    | ItemGet (Maybe DeckSource)
    | Shuffle
    | RandomGot (List Float)


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
                    {model | message = "getItem failed."} ! []

                Just deckSource ->
                    {model
                        | deck = List.map Dict.fromList deckSource
                        , message = "Deck is loaded."} ! []

        OnDrop [file] ->
            model ! [getFileContents file]

        FileContentsGot (Ok csv) ->
            let
                deckSource = parseCSV csv
            in
                {model | deck = List.map Dict.fromList deckSource} ! [LS.setDeckReq ("DEV-1", deckSource)]

        Shuffle ->
            model ! [getShuffleSource <| List.length model.deck]

        RandomGot source ->
            {model | deck = shuffle source model.deck} ! []

        _ -> model ! []


getFileContents : NativeFile -> Cmd Msg
getFileContents file =
    FileReader.readAsTextFile file.blob
        |> Task.attempt FileContentsGot


getShuffleSource : Int -> Cmd Msg
getShuffleSource n =
    let
        gen = Random.list n (Random.float 0 1)
    in
        Random.generate RandomGot gen


shuffle : List Float -> List a -> List a
shuffle source seq =
        zip seq source
        |> List.sortBy Tuple.second
        |> List.map Tuple.first


---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "デッキをシャッフルしてみるやつ" ]
        , div []
            [ Html.input
                [ Attr.type_ "file"
                , FileReader.onFileChange OnDrop
                , Attr.multiple False
                ] []
            ]
        , p [] [ text model.message ]
        , Html.button [ Events.onClick Shuffle ] [ text "シャッフルする" ]
        , deckTable model.deck
        ]


deckTable : Deck -> Html Msg
deckTable deck =
    let
        cells =
            List.map (\dict ->
                tr []
                    [ td [] [  Dict.get "ID"   dict |>  Maybe.withDefault "" |> text ]
                    , td [] [  Dict.get "level" dict |>  Maybe.withDefault "" |> (\s -> "Lv" ++ s) |> text ]
                    , td [] [  Dict.get "mana" dict |>  Maybe.withDefault "" |> (\s -> s ++ "mana") |> text ]
                    , td [] [  Dict.get "card_name" dict |>  Maybe.withDefault "" |> text ]
                    ]) deck
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


parseCSV : String -> DeckSource
parseCSV csvStr =
    let
        csv = String.toList csvStr

        (rawHeader, _, rawBody) =
            split (\c -> c == '\n') csv

        header =
            String.fromList rawHeader
            |> String.split ","
            |> List.map String.trim

        parse body =
            case parseCSVRecord body of
                (head, []) ->
                    [head]

                (head, tail) ->
                    head :: parse tail
    in
        parse rawBody
        |> List.map (String.fromList >> String.split "," >> List.map String.trim >> zip header)


parseCSVRecord : List Char -> (List Char, List Char)
parseCSVRecord unparsed =
    let
        (parsed, pivot, rest) =
            split (\c -> c == '\n' || c == '"') unparsed
    in
        if pivot == Just '"' then
            let
                (quoted, _, rest_) =
                    split (\c -> c == '"') rest

                (following, rest__) =
                    parseCSVRecord rest_
            in
                (parsed ++ quoted ++ following, rest__)
        else
            (parsed, rest)


split : (a -> Bool) -> List a -> (List a, Maybe a, List a)
split pred seq =
    let
        split_ buf rest =
            case rest of
                [] ->
                    (List.reverse buf, Nothing, [])
                x :: xs ->
                    if pred x then
                        (List.reverse buf, Just x, xs)
                    else
                        split_ (x :: buf) xs
    in
        split_ [] seq


zip : List a -> List b -> List (a, b)
zip seq1 seq2 =
    List.map2 (,) seq1 seq2
