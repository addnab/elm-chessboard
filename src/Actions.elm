module Actions exposing (Action(..))

import Chess.Types exposing (Square, Position, Move, PlayerPiece)

type Action
  = Reset
  | MovePiece Position Move
  | PickPromotionPiece Position
  | PromotePawn PlayerPiece Position
  | Select Square
  | Deselect
