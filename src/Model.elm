module Model exposing (Model, initial)

import Dict
import Chess.Board exposing (Board, Rank)
import Chess.Pieces exposing (Piece(..), PlayerPiece)
import Chess.Square exposing (Square, Highlight(..))
import Chess.Position exposing (..)
import Chess.Players exposing (Player(..), PlayerInfo, playerInit)

type alias Model =
  { board : Board
  , boardView : Board
  , selectedSquare : Maybe Square
  , playerInTurn : Player
  , playersInfo :
      { white : PlayerInfo
      , black : PlayerInfo
      }
  }

createSquare : Int -> Int -> Maybe PlayerPiece -> Square
createSquare rank file piece =
  Square piece { file = file, rank = rank } None

createRank : Int -> List (Maybe PlayerPiece) -> Rank
createRank rank pieceOrder =
  pieceOrder
    |> List.map2 (createSquare rank) [1..8]
    |> List.map2 (,) [1..8]
    |> Dict.fromList

emptyRank : Int -> Rank
emptyRank rank =
  List.repeat 8 Nothing
    |> createRank rank

initialFirstRank : Player -> Rank
initialFirstRank player =
  let
    pieceOrder = List.map (PlayerPiece player False) [ R, N, B, Q, K, B, N, R ]
    rank =
      case player of
        White -> 1
        Black -> 8
  in
    pieceOrder
      |> List.map Just
      |> createRank rank

initialSecondRank : Player -> Rank
initialSecondRank player =
  let
    rank =
      case player of
        White -> 2
        Black -> 7
    pieceOrder = List.map (PlayerPiece player False) [ P, P, P, P, P, P, P, P ]
  in
    pieceOrder
      |> List.map Just
      |> createRank rank

initial : Model
initial =
  let
    board =
      Dict.fromList
        [ (1, initialFirstRank White)
        , (2, initialSecondRank White)
        , (3, emptyRank 3)
        , (4, emptyRank 4)
        , (5, emptyRank 5)
        , (6, emptyRank 6)
        , (7, initialSecondRank Black)
        , (8, initialFirstRank Black)
        ]
  in
    { board = board
    , boardView = board
    , selectedSquare = Nothing
    , playerInTurn = White
    , playersInfo =
        { white = playerInit (Position 5 1)
        , black = playerInit (Position 5 8)
        }
    }
