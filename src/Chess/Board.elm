module Chess.Board exposing (Board)

import Dict as Dict
import Chess.Square exposing (Square)

type alias Board = Dict.Dict Int (Dict.Dict Int Square)
