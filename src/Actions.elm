module Actions exposing (Action(..))

import Chess.Types exposing (Square, Position, Move)

type Action
  = Reset
  | MovePiece Position Move
  | Select Square
  | Deselect
