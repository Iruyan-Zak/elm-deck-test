module Main exposing (..)

import Dict exposing (Dict)
import FileReader exposing (NativeFile)
import Html
import Html exposing (Html, div, table, tr, td, h1, h2, p, text)
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
    { fileSet : Bool
    , deck : Deck
    , newDeckName : String
    , deckNames : List String
    , message : String
    }


init : ( Model, Cmd Msg )
init =
    Model False [] "" [] "Deck is not loaded."
        ! [LS.getDeckNamesReq ()]


---- UPDATE ----


type Msg
    = NoOp
    | OnDrop (List NativeFile)
    | FileContentsGot (Result FileReader.Error String)
    | ItemGet (Maybe DeckSource)
    | Shuffle
    | RandomGot (List Float)
    | ChangenewDeckName String
    | Register
    | DeckNamesGot (List String)
    | LoadDeck String
    | RemoveDeck String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ LS.getDeckRes ItemGet
        , LS.getDeckNamesRes DeckNamesGot
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                {model
                    | deck = List.map Dict.fromList deckSource
                    , fileSet = True
                    } ! []

        Shuffle ->
            model ! [getShuffleSource <| List.length model.deck]

        RandomGot source ->
            {model | deck = shuffle source model.deck} ! []

        ChangenewDeckName newDeckName ->
            {model | newDeckName = newDeckName} ! []

        Register ->
            { model
                | newDeckName = ""
                , fileSet = False
                } ! [ LS.setDeckReq (model.newDeckName, List.map Dict.toList model.deck) ]

        DeckNamesGot names ->
            {model | deckNames = names} ! []

        LoadDeck deckName ->
            model ! [ LS.getDeckReq deckName ]

        RemoveDeck deckName ->
            model ! [ LS.removeDeckReq deckName ]

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
        [ Html.header [class "container"] [ h1 [] [ text "デッキをシャッフルしてみるやつ" ] ]
        , div [class "container"]
            [ p [] [ text model.message ]
            , div [class "row"]
                [ div [class "col-md-8"]
                    [  h2 [] [ text "ドローテスト" ]
                    , div [] [ Html.button [ class "c-btn",  Events.onClick Shuffle ] [ text "シャッフルする" ] ]
                    , div [] [ deckTable model.deck ]
                    ]
                , div [class "col-md-4"]
                    [ div [class "c-pane"]
                        [ h2 [] [ text "デッキを登録する" ]
                        , Html.input
                            [ class "form-control-file"
                            , Attr.type_ "file"
                            , FileReader.onFileChange OnDrop
                            , Attr.multiple False
                            ] []
                        ,  div []
                            [ Html.label [Attr.for "register_deck"] [ text "デッキ名" ]
                            , Html.input
                                [ Attr.id "register_deck"
                                , class "form-control"
                                , Attr.placeholder "1文字以上のデッキ名"
                                , Events.onInput ChangenewDeckName
                                , Events.onBlur <| ChangenewDeckName model.newDeckName
                                ] []
                            ]
                        , Html.button
                            [ class "c-btn"
                            , Html.Attributes.disabled
                                <| String.length model.newDeckName == 0 || not model.fileSet
                            , Events.onClick Register
                            ] [ text "登録する" ]
                        ]
                    , div [class "c-pane"]
                        [ h2 [] [ text "デッキを読み込む" ]
                        , deckNameList model.deckNames
                        ]
                    ]
                ]
            ]
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
        table [class "table"] cells


deckNameList : List String -> Html Msg
deckNameList names =
    let
        toRow name =
            tr []
                [ td [ class "c-clickable-cell", Events.onClick (LoadDeck name) ] [ text name ]
                , td [ class "c-clickable-cell", Events.onClick (RemoveDeck name) ] [ text "×"]
                ]
    in
        table [class "table"] <| List.map toRow names

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
