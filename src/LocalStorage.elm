port module LocalStorage exposing (..)


type alias DeckSource =
    List (List (String, String))


port getDeckReq : String -> Cmd msg
port setDeckReq : (String, DeckSource) -> Cmd msg
port removeDeckReq : String -> Cmd msg
port getDeckNamesReq : () -> Cmd msg
port getDeckRes : (Maybe DeckSource -> msg) -> Sub msg
port getDeckNamesRes : (List String -> msg) -> Sub msg
