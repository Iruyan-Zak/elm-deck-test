port module LocalStorage exposing (..)
import Json.Encode exposing (Value)


type alias Record =
    { id : String
    , name : String
    }


port getDeckReq : String -> Cmd msg
port setDeckReq : (String, List Record) -> Cmd msg
port getDeckRes : (Maybe (List Record) -> msg) -> Sub msg
port setDeckRes : (() -> msg) -> Sub msg
