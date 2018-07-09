port module LocalStorage exposing (..)
import Dict exposing (Dict)

type alias Record =
    { id : String
    , name : String
    }

type alias DeckSource =
    List (List (String, String))

port getDeckReq : String -> Cmd msg
port setDeckReq : (String, DeckSource) -> Cmd msg
port getDeckRes : (Maybe DeckSource -> msg) -> Sub msg
port setDeckRes : (() -> msg) -> Sub msg
