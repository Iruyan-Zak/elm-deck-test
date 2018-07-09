port module LocalStorage exposing (..)
import Json.Encode exposing (Value)


port getItemReq : String -> Cmd msg
port setItemReq : (String, Value) -> Cmd msg
port getItemRes : (Maybe Value -> msg) -> Sub msg
port setItemRes : (() -> msg) -> Sub msg
