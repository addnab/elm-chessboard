module Chess.Board exposing (Board, Rank)

import Dict as Dict
import Chess.Square exposing (Square)

type alias Board = Dict.Dict Int Rank

type alias Rank = Dict.Dict Int Square
