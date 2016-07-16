module Model exposing (Model, initial)

import Dict
import Chess.Board exposing (Board)
import Chess.Pieces exposing (Piece(..), PlayerPiece)
import Chess.Square exposing (Square)
import Chess.Position exposing (..)
import Chess.Players exposing (Player(..))

type alias Model =
  { board : Board
  , selectedSquare: Maybe Square
  }

createSquare : Rank -> File -> Maybe PlayerPiece -> Square
createSquare rank file piece =
  Square piece { file = file, rank = rank } Nothing

createRank : Rank -> List (Maybe PlayerPiece) -> Dict.Dict Int Square
createRank rank pieceOrder =
  pieceOrder
    |> List.map2 (createSquare rank) [1..8]
    |> List.map2 (,) [1..8]
    |> Dict.fromList

emptyRank : Rank -> Dict.Dict Int Square
emptyRank rank =
  List.repeat 8 Nothing
    |> createRank rank

initialFirstRank : Player -> Dict.Dict Int Square
initialFirstRank player =
  let
    pieceOrder = List.map (PlayerPiece player) [ R, N, B, K, Q, B, N, R ]
    rank =
      case player of
        White -> 1
        Black -> 8
  in
    pieceOrder
      |> List.map Just
      |> createRank rank

initialSecondRank : Player -> Dict.Dict Int Square
initialSecondRank player =
  let
    rank =
      case player of
        White -> 2
        Black -> 7
    pieceOrder = List.map (PlayerPiece player) [ P, P, P, P, P, P, P, P ]
  in
    pieceOrder
      |> List.map Just
      |> createRank rank

initial : Model
initial =
  { board = Dict.fromList
      [ (1, initialFirstRank White)
      , (2, initialSecondRank White)
      , (3, emptyRank 3)
      , (4, emptyRank 4)
      , (5, emptyRank 5)
      , (6, emptyRank 6)
      , (7, initialSecondRank Black)
      , (8, initialFirstRank Black)
      ]
  , selectedSquare = Nothing
  }
