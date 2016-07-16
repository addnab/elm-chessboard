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

createRank : Rank -> List (Maybe PlayerPiece) -> List Square
createRank rank pieceOrder =
  List.map2
    (createSquare rank)
    [1..8]
    pieceOrder

emptyRank : Rank -> Dict.Dict Int Square
emptyRank rank =
  Dict.fromList
    <| List.map2 (,) [1..8]
      <| createRank rank
        <| List.repeat 8 Nothing

initialFirstRank : Player -> Dict.Dict Int Square
initialFirstRank player =
  let
    pieceOrder = List.map (PlayerPiece player) [ R, N, B, K, Q, B, N, R ]
    rankNumber =
      case player of
        White -> 1
        Black -> 8
  in
    Dict.fromList
      <| List.map2 (,) [1..8]
        <| createRank rankNumber
          <| List.map Just pieceOrder

initialSecondRank : Player -> Dict.Dict Int Square
initialSecondRank player =
  let
    rankNumber =
      case player of
        White -> 2
        Black -> 7
    pieceOrder = List.map (PlayerPiece player) [ P, P, P, P, P, P, P, P ]
  in
    Dict.fromList
      <| List.map2 (,) [1..8]
        <| createRank rankNumber
          <| List.map Just pieceOrder

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
